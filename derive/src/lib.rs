use std::borrow::Cow;

use ident_case::RenameRule;
use proc_macro2::{Ident, Literal, TokenStream, Span};
use quote::{format_ident, quote};
use syn::{parse_macro_input, DeriveInput};

#[proc_macro_derive(Spec)]
pub fn derive_spec(input: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    trait_and_impl(&input).into()
}

fn trait_and_impl(
    DeriveInput {
        vis,
        ident,
        generics,
        data,
        ..
    }: &DeriveInput,
) -> TokenStream {
    let trait_name = format_ident!("{}Fields", ident);
    let fields: Vec<_> = match data {
        syn::Data::Struct(syn::DataStruct { fields, .. }) => {
            fields.iter().enumerate().map(struct_fns).collect()
        }
        syn::Data::Enum(syn::DataEnum { variants, .. }) => variants
            .iter()
            .map(|v| enum_fns(ident, v))
            .collect(),
        syn::Data::Union(syn::DataUnion { fields, .. }) => {
            fields.named.iter().map(union_fns).collect()
        }
    };
    let (trait_fns, impl_fns): (Vec<_>, Vec<_>) = fields.into_iter().map(FnTokens::into_tuple).unzip();
    let (_impl_generics, ty_generics, where_clause) = generics.split_for_impl();
    let lts = generics.lifetimes();
    let tps = generics.type_params();
    let cps = generics.const_params();

    quote! {
        #vis trait #trait_name #generics {
            #(#trait_fns)*
        }

        impl<'spec, #(#lts),* #(#tps),* #(#cps),*> #trait_name #ty_generics
            for ::speculoos::Spec<'spec, #ident #ty_generics> #where_clause {
            #(#impl_fns)*
        }
    }
}

struct FnTokens {
    trait_tokens: TokenStream,
    impl_tokens: TokenStream,
}

impl FnTokens {
    fn new(trait_tokens: TokenStream, impl_tokens: TokenStream) -> Self {
        FnTokens { trait_tokens, impl_tokens }
    }

    fn into_tuple(self) -> (TokenStream, TokenStream) {
        (self.trait_tokens, self.impl_tokens)
    }
}

/// Returns tokens defining the trait and impl of:
///
/// ```rust,no_compile
/// fn field_name(&mut self, f: impl FnOnce(&mut Spec<'_, field_type>)) -> &mut Self {
///     let value = &self.subject.field_name;
///     f(&mut ...);
///     self
/// }
/// ```
fn struct_fns((i, field): (usize, &syn::Field)) -> FnTokens {
    let syn::Field { ident, ty, .. } = field;
    let fn_name = ident.clone().unwrap_or_else(|| format_ident!("_{}", i));

    let signature = quote! {
        fn #fn_name(
            &mut self, f: impl ::std::ops::FnOnce(&mut ::speculoos::Spec<'_, #ty>)) -> &mut Self
    };

    let name_ident = Ident::new("name", Span::mixed_site());
    let value_ident = Ident::new("value", Span::mixed_site());

    let name_decl = {
        let ident = if let Some(ident) = ident {
            ident.to_string()
        } else {
            i.to_string()
        };

        name_decl(&name_ident, &ident)
    };
    let spec_expr = spec_expr(&name_ident, &value_ident);

    let impl_fn = quote! {
        #signature {
            let #value_ident = &self.subject.#ident;
            #name_decl

            f(#spec_expr);

            self
        }
    };

    FnTokens::new(quote! { #signature; }, impl_fn)
}

/// Returns tokens defining the trait and impl of:
///
/// ```rust,example
/// fn is_variant_name(&mut self, f: impl FnOnce(&mut Spec<'_, variant_types...>)) {
///     if let T::V { a, b } = self.subject {
///         f(&mut ..., &mut ...);
///     } else {
///         panic!("...")
///     }
/// }
///
/// fn as_variant_name(&mut self, f: impl FnOnce(&mut Spec<'_, variant_types...>)) -> &mut Self {
///     if let T::V { a, b } = self.subject {
///         f(&mut ..., &mut ...);
///     }
///
///     self
/// }
/// ```
///
/// If the variant is empty, the following trait and impl will be generated
/// instead:
///
/// ```rust,example
/// fn is_variant_name(&mut self) {
///     if let T::V {} = self.subject {
///         // No-op.
///     } else {
///         panic!("...")
///     }
/// }
/// ```
fn enum_fns(
    ty: &Ident,
    syn::Variant { ident, fields, .. }: &syn::Variant,
) -> FnTokens {
    let fn_name = Ident::new(&RenameRule::SnakeCase.apply_to_variant(ident.to_string()), ident.span());
    let is_fn_name = format_ident!("is_{}", fn_name);
    let as_fn_name = format_ident!("as_{}", fn_name);

    if fields.is_empty() {
        let pattern = match fields {
            syn::Fields::Named(_) => quote! { {} },
            syn::Fields::Unnamed(_) => quote! { () },
            syn::Fields::Unit => quote! {},
        };

        return FnTokens::new(
            quote! {
                fn #is_fn_name(&mut self);
            },
            quote! {
                fn #is_fn_name(&mut self) {
                    if let #ty::#ident #pattern = self.subject {
                        // Success.
                    } else {
                        todo!()
                    }
                }
            },
        );
    }

    let mut name_decls = Vec::with_capacity(fields.len());
    let mut spec_tys = Vec::with_capacity(fields.len());
    let mut spec_exprs = Vec::with_capacity(fields.len());
    let mut field_names = Vec::with_capacity(fields.len());

    for (i, syn::Field { ident, ty, .. }) in fields.iter().enumerate() {
        let (pattern_ident, field_ident) = match ident {
            Some(ident) => (Cow::Borrowed(ident), ident.to_string()),
            None => (Cow::Owned(format_ident!("v{i}")), i.to_string()),
        };
        let name_ident = format_ident!("{}_name", pattern_ident);

        name_decls.push(name_decl(&name_ident, &field_ident));
        spec_tys.push(quote! { ::speculoos::Spec<'_, #ty> });
        spec_exprs.push(spec_expr(&name_ident, &pattern_ident));
        field_names.push(pattern_ident);
    }

    let pattern = match fields {
        syn::Fields::Named(_) => quote! { { #(#field_names,)* } },
        syn::Fields::Unnamed(_) => quote! { ( #(#field_names,)* ) },
        syn::Fields::Unit => unreachable!(),
    };

    let inner_expr = quote! {
        #(#name_decls)*
        f( #(#spec_exprs,)* );
    };

    let is_signature = quote! {
        fn #is_fn_name(&mut self, f: impl ::std::ops::FnOnce(#(&mut #spec_tys,)*))
    };
    let as_signature = quote! {
        fn #as_fn_name(&mut self, f: impl ::std::ops::FnOnce(#(&mut #spec_tys,)*)) -> &mut Self
    };

    let trait_fns = quote! {
        #is_signature;
        #as_signature;
    };
    let impl_fns = quote! {
        #is_signature {
            if let #ty::#ident #pattern = self.subject {
                #inner_expr
            } else {
                todo!()
            }
        }

        #as_signature {
            if let #ty::#ident #pattern = self.subject {
                #inner_expr
            }

            self
        }
    };

    FnTokens::new(trait_fns, impl_fns)
}

/// Returns tokens defining the trait and impl of:
///
/// ```rust,example
/// unsafe fn field_name(&mut self, f: impl FnOnce(&mut Spec<'_, field_type>)) {
///     let value = &self.subject.field_name;
///     f(&mut ...);
/// }
/// ```
fn union_fns(syn::Field { ident, ty, .. }: &syn::Field) -> FnTokens {
    let ident = ident.as_ref().unwrap();
    let fn_name = ident;

    let signature = quote! {
        unsafe fn #fn_name(&mut self, f: impl ::std::ops::FnOnce(&mut ::speculoos::Spec<'_, #ty>))
    };

    let name_ident = Ident::new("name", Span::mixed_site());
    let value_ident = Ident::new("value", Span::mixed_site());

    let name_decl = name_decl(&name_ident, &fn_name.to_string());
    let spec_expr = spec_expr(&name_ident, &value_ident);

    let impl_fn = quote! {
        #signature {
            let #value_ident = &self.subject.#ident;
            #name_decl

            f(#spec_expr);
        }
    };

    FnTokens::new(quote! { #signature; }, impl_fn)
}

/// Returns tokens creating a `spec` value with the given name and value.
fn spec_expr(name_ident: &Ident, value_ident: &Ident) -> TokenStream {
    quote! {
        &mut self.clone().map(|_| #value_ident).named(#name_ident)
    }
}

/// Returns tokens defining `name_ident` to be a `&str` representing the current
/// `Spec` name followed by the specified `field_name`.
fn name_decl(name_ident: &Ident, field_name: &str) -> TokenStream {
    let field_name_str = Literal::string(field_name);
    let field_name_with_prefix_str = Literal::string(&format!("{{}}.{field_name}"));

    quote! {
        let #name_ident = if let ::std::option::Option::Some(name) = self.subject_name {
            ::std::borrow::Cow::Owned(format!(#field_name_with_prefix_str, name))
        } else {
            ::std::borrow::Cow::Borrowed(#field_name_str)
        };
        let #name_ident = #name_ident.as_ref();
    }
}
