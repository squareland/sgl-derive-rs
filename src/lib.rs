extern crate proc_macro;

use proc_macro::TokenStream;
use quote::{quote, quote_spanned, ToTokens};
use syn::{parse_macro_input, Expr, Fields, ItemStruct, Lit, Meta};
use syn::spanned::Spanned;

#[proc_macro_attribute]
pub fn vertex(_args: TokenStream, input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as ItemStruct);
    let vis = input.vis.to_token_stream();
    let name = input.ident;
    let mut fields = Vec::new();
    let mut attributes = Vec::new();
    let mut enable = Vec::new();
    let mut disable = Vec::new();
    let mut constructor_args = Vec::new();
    let mut converters = Vec::new();
    let mut positions = 0;
    let mut normals = 0;
    let mut colors = 0;
    match &input.fields {
        Fields::Named(fsn) => {
            for (i, f) in fsn.named.iter().enumerate() {
                let ident = f.ident.as_ref().unwrap().to_token_stream();
                let vis = f.vis.to_token_stream();
                let field_name = ident.to_string();
                let ty = f.ty.to_token_stream();
                let count = f.attrs.len();
                if count == 0 {
                    return quote_spanned! {
                        f.span() =>
                        compile_error!("vertex fields must have a #[usage]");
                    }.into();
                } else if count != 1 {
                    return quote_spanned! {
                        f.span() =>
                        compile_error!("vertex fields must have exactly one attribute");
                    }.into();
                }
                for a in &f.attrs {
                    match &a.meta {
                        Meta::Path(p) => {
                            let key = p.get_ident().unwrap().to_string();
                            match key.as_str() {
                                "position" => {
                                    if positions > 0 {
                                        return quote_spanned! {
                                            a.span() =>
                                            compile_error!("vertex must have exactly one #[position]");
                                        }.into();
                                    }
                                    positions += 1;
                                    enable.push(quote! {
                                        let offset = sgl::offset_of!(Self => #ident);
                                        let usage = sgl::tessellator::ElementUsage::Position;
                                        usage.begin::<#ty>(size as i32, offset.apply_ptr(start).cast());
                                    });
                                    disable.push(quote! {
                                        let usage = sgl::tessellator::ElementUsage::Position;
                                        usage.end();
                                    });
                                }
                                "normal" => {
                                    if normals > 0 {
                                        return quote_spanned! {
                                            a.span() =>
                                            compile_error!("vertex can only have one #[normal]");
                                        }.into();
                                    }
                                    normals += 1;
                                    enable.push(quote! {
                                        let offset = sgl::offset_of!(Self => #ident);
                                        let usage = sgl::tessellator::ElementUsage::Normal;
                                        usage.begin::<#ty>(size as i32, offset.apply_ptr(start).cast());
                                    });
                                    disable.push(quote! {
                                        let usage = sgl::tessellator::ElementUsage::Normal;
                                        usage.end();
                                    });
                                }
                                "color" => {
                                    if colors > 0 {
                                        return quote_spanned! {
                                            a.span() =>
                                            compile_error!("vertex can only have one #[color]");
                                        }.into();
                                    }
                                    colors += 1;
                                    enable.push(quote! {
                                        let offset = sgl::offset_of!(Self => #ident);
                                        let usage = sgl::tessellator::ElementUsage::Color;
                                        usage.begin::<#ty>(size as i32, offset.apply_ptr(start).cast());
                                    });
                                    disable.push(quote! {
                                        let usage = sgl::tessellator::ElementUsage::Color;
                                        usage.end();
                                    });
                                }
                                "texture" => {
                                    return quote_spanned! {
                                        a.span() =>
                                        compile_error!("#[texture] attribute must have an index value");
                                    }.into();
                                }
                                "generic" => {
                                    return quote_spanned! {
                                        a.span() =>
                                        compile_error!("#[generic] attribute must have an index value");
                                    }.into();
                                }
                                _ => {
                                    return quote_spanned! {
                                        a.span() =>
                                        compile_error!("unsupported element usage");
                                    }.into();
                                }
                            }
                        }
                        Meta::NameValue(nv) => {
                            let index = if let Expr::Lit(e) = &nv.value {
                                if let Lit::Int(i) = &e.lit {
                                    i.base10_parse::<u32>().unwrap()
                                } else {
                                    return quote_spanned! {
                                        a.span() =>
                                        compile_error!("only integer literals are supported as element usage index");
                                    }.into();
                                }
                            } else {
                                return quote_spanned! {
                                    a.span() =>
                                    compile_error!("only literals are supported as element usage index");
                                }.into();
                            };

                            let key = nv.path.get_ident().unwrap().to_string();
                            match key.as_str() {
                                "texture" => {
                                    enable.push(quote! {
                                        let offset = sgl::offset_of!(Self => #ident);
                                        let usage = sgl::tessellator::ElementUsage::Texture(#index as u32);
                                        usage.begin::<#ty>(size as i32, offset.apply_ptr(start).cast());
                                    });
                                    disable.push(quote! {
                                        let usage = sgl::tessellator::ElementUsage::Texture(#index as u32);
                                        usage.end();
                                    });
                                }
                                "generic" => {
                                    enable.push(quote! {
                                        let offset = sgl::offset_of!(Self => #ident);
                                        let usage = sgl::tessellator::ElementUsage::Generic(#index as u32);
                                        usage.begin::<#ty>(size as i32, offset.apply_ptr(start).cast());
                                    });
                                    disable.push(quote! {
                                        let usage = sgl::tessellator::ElementUsage::Generic(#index as u32);
                                        usage.end();
                                    });
                                }
                                _ => {
                                    return quote_spanned! {
                                        a.span() =>
                                        compile_error!("unsupported key=value element usage");
                                    }.into();
                                }
                            }
                        },
                        _ => {
                            return quote_spanned! {
                                a.span() =>
                                compile_error!("unsupported meta");
                            }.into();
                        }
                    }
                }
                fields.push(quote! {
                    #vis #ident: #ty,
                });
                constructor_args.push(quote! {
                    #ident: impl Into<#ty>
                });
                converters.push(quote! {
                    #ident: #ident.into()
                });
                attributes.push(quote! {
                    {
                        const CNAME: &[u8] = concat!(#field_name, "\0").as_bytes();
                        let name = unsafe { std::ffi::CStr::from_bytes_with_nul_unchecked(CNAME) };
                        let attribute = program.attribute::<#ty>(name)
                            .ok_or(sgl::shader::ProgramError::AttributeMissing(#field_name))?;
                        attribute.bind(#i as u32);
                    }
                });
            }
        }
        _ => panic!("#[vertex] can only be derived for structs with named fields"),
    }
    let expanded = quote! {
        #[repr(C)]
        #[derive(Copy, Clone, Debug)]
        #vis struct #name {
            #(#fields)*
        }
        
        unsafe impl sgl::NoUninit for #name {}
        
        impl sgl::tessellator::Vertex for #name {
            unsafe fn enable_client_state(start: *const #name) {
                let size = std::mem::size_of::<Self>();
                #(#enable)*
            }

            unsafe fn disable_client_state() {
                #(#disable)*
            }

            #[inline(always)]
            fn bind_attributes(program: &sgl::shader::LinkedProgramId<Self>) -> Result<(), sgl::shader::ProgramError> {
                unsafe {
                    #(#attributes)*
                }
                Ok(())
            }
        }

        impl #name {
            pub fn new(#(#constructor_args),*) -> Self {
                Self {
                    #(#converters),*
                }
            }
        }
    };
    TokenStream::from(expanded)
}

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