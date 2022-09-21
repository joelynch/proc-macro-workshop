#![feature(let_chains)]

use core::panic;

use proc_macro::TokenStream;
use proc_macro2::{Ident, Span, TokenStream as TokenStream2};
use quote::{quote, quote_spanned, ToTokens};
use syn::{
    parse_macro_input, punctuated::Iter, spanned::Spanned, AngleBracketedGenericArguments, Data,
    DeriveInput, Field, Fields, GenericArgument, Path, PathArguments, Type, TypePath,
};

#[proc_macro_derive(Builder)]
pub fn derive(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);

    let builder_struct = build_builder_struct(&input);
    let builder_struct_tokens = builder_struct.to_token_stream();

    let original_struct_impl = build_original_struct_impl(&input, &builder_struct);
    let builder_impl = build_builder_impl(&input, &builder_struct);

    quote! {
        #original_struct_impl
        #builder_struct_tokens
        #builder_impl
    }
    .into()
}

fn build_builder_struct(input: &DeriveInput) -> DeriveInput {
    let mut out = input.clone();
    for field in named_struct_fields_mut(&mut out) {
        if !is_optional(field) {
            make_optional(field);
        }
    }
    out.ident = builder_name(&input.ident);
    out
}

fn build_original_struct_impl(
    original_struct: &DeriveInput,
    builder: &DeriveInput,
) -> TokenStream2 {
    let name = original_struct.ident.clone();
    let builder_name = builder.ident.clone();
    let field_inits = named_struct_fields(&original_struct).map(|f| {
        let var_name = f.ident.clone();
        quote_spanned! {f.span()=>  #var_name: None, }
    });
    quote! {
        impl #name {
            pub fn builder() -> #builder_name {
                #builder_name {
                    #(#field_inits)*
                }
            }
        }
    }
}

fn build_builder_impl(original_struct: &DeriveInput, builder: &DeriveInput) -> TokenStream2 {
    let original_name = original_struct.ident.clone();
    let name = builder.ident.clone();
    let setters = named_struct_fields(original_struct).map(|f| {
        let mut new_f = f.clone();
        let field_name = f.ident.clone();
        if is_optional(f) {
            make_non_optional(&mut new_f);
        }
        let field_type = new_f.ty.clone();
        quote_spanned! {f.span()=>
            pub fn #field_name(&mut self, #field_name: #field_type) -> &mut Self {
                self.#field_name = Some(#field_name);
                self
            }
        }
    });
    let fields = named_struct_fields(original_struct).map(|f| {
        let field_name = f.ident.clone();
        if is_optional(f) {
            quote_spanned! {f.span()=>
                #field_name: self.#field_name.clone()
            }
        } else {
            quote_spanned! {f.span()=>
                #field_name: self.#field_name
                    .clone()
                    .ok_or_else(|| Box::<dyn std::error::Error>::from(
                        format!("{} was not set", stringify!(#field_name))
                    ))?,
            }
        }
    });
    quote! {
        impl #name {
            #(#setters)*

            pub fn build(&mut self) -> Result<#original_name, Box<dyn std::error::Error>> {
                let built = #original_name {
                    #(#fields)*
                };
                Ok(built)
            }
        }
    }
}

fn builder_name(name: &Ident) -> Ident {
    Ident::new(&format!("{}Builder", name), Span::call_site())
}

fn make_optional(field: &mut Field) {
    let ty = &field.ty;
    field.ty = Type::Verbatim(quote! { Option<#ty> }.into());
}

fn is_optional(field: &Field) -> bool {
    if let Type::Path(TypePath {
        path: Path { ref segments, .. },
        qself: None,
    }) = field.ty
    {
        if segments.len() > 0 {
            if segments.first().unwrap().ident.to_string() == "Option" {
                return true;
            }
        }
    }
    false
}

fn make_non_optional(field: &mut Field) {
    if let Type::Path(TypePath {
        path: Path { ref segments, .. },
        qself: None,
    }) = field.ty
    && let Some(segment) = segments.first()
    && (segment.ident.to_string() == "Option")
    && let PathArguments::AngleBracketed(AngleBracketedGenericArguments {ref args, ..}) = segment.arguments
    && let Some(GenericArgument::Type(inner_type)) = args.first()
    {
        field.ty = inner_type.clone();
    }
}

fn named_struct_fields_mut(input: &mut DeriveInput) -> syn::punctuated::IterMut<Field> {
    if let Data::Struct(ref mut s) = input.data {
        if let Fields::Named(ref mut fields) = s.fields {
            fields.named.iter_mut()
        } else {
            panic!("cannot derive builder for struct with unnamed args");
        }
    } else {
        panic!("cannot derive builder for non struct");
    }
}

fn named_struct_fields(input: &DeriveInput) -> Iter<Field> {
    if let Data::Struct(ref s) = input.data {
        if let Fields::Named(ref fields) = s.fields {
            fields.named.iter()
        } else {
            panic!("cannot derive builder for struct with unnamed args");
        }
    } else {
        panic!("cannot derive builder for non struct");
    }
}
