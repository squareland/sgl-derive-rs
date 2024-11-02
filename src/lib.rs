extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, ToTokens};
use syn::{parse_macro_input, Fields, ItemStruct};

#[proc_macro_attribute]
pub fn program(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemStruct);
    let vis = input.vis.to_token_stream();
    let name = input.ident;
    let mut fields = Vec::new();
    let mut initializers = Vec::new();
    let mut methods = Vec::new();
    match &input.fields {
        Fields::Named(fsn) => {
            for (i, f) in fsn.named.iter().enumerate() {
                let ident = f.ident.as_ref().unwrap().to_token_stream();
                let ty = f.ty.to_token_stream();
                let field_name = ident.to_string();
                fields.push(quote!(
                    #ident: i32,
                ));
                initializers.push(quote! {
                    linked.uniform::<#ty>({
                        const CNAME: &[u8] = concat!(#field_name, "\0").as_bytes();
                        eprintln!("{:?}", CNAME);
                        unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(CNAME) }
                    })
                    .ok_or(ProgramError::UniformMissing(#field_name))?.into_inner() as i32,
                });
                methods.push(quote! {
                    pub fn #ident(&self, value: #ty) {
                        <#ty as UniformValue>::set(value, self.__uniforms[#i]);
                    }
                })
            }
        }
        Fields::Unnamed(e) => panic!("#[program] can only be derived for structs with named fields"),
        Fields::Unit => {}
    }
    let uniforms = fields.len();

    let expanded = quote! {
        #vis struct #name<V> {
            __inner: sgl::shader::LinkedProgramId<V>,
            __uniforms: [i32; #uniforms],
        }

        {
            use std::ffi::CStr;
            use sgl::state::GraphicsContext;
            use sgl::shader::{ProgramError, UniformValue};
            use sgl::tessellator::Vertex;

            impl<V> sgl::shader::Program for #name<V> where V: Vertex {
                type Format = V;

                fn from_source(context: GraphicsContext, vertex: &str, fragment: &str) -> Result<Self, ProgramError> {
                    let linked = sgl::shader::link_program::<V>(context, vertex, fragment)?;
                    // Safety: uniform locations are stored in our struct and are guaranteed
                    // not to outlive the linked program by being private fields
                    Ok(unsafe {
                        Self {
                            __uniforms: [#(#initializers)*],
                            __inner: linked,
                        }
                    })
                }
            }

            impl<V> #name<V> {
                #(#methods)*
            }
        }
    };

    TokenStream::from(expanded)
}