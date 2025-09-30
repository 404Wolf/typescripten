use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::Module;

fn generate_program(context: &Context) -> Module {
    let module = context.create_module("stdin_adder");
    let builder = context.create_builder();

    // Types
    let i32_type = context.i32_type();
    let i8_type = context.i8_type();
    let i8_ptr_type = i8_type.ptr_type(AddressSpace::default());

    // External function declarations
    let scanf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
    let scanf_fn = module.add_function("scanf", scanf_type, None);

    let printf_type = i32_type.fn_type(&[i8_ptr_type.into()], true);
    let printf_fn = module.add_function("printf", printf_type, None);

    // String constants
    let input_format = "%d %d\0";
    let output_format = "%d\n\0";
    // Main function
    let main_type = i32_type.fn_type(&[], false);
    let main_fn = module.add_function("main", main_type, None);
    let entry_block = context.append_basic_block(main_fn, "entry");

    builder.position_at_end(entry_block);

    let input_format_global = builder
        .build_global_string_ptr(input_format, ".input_fmt")
        .unwrap();
    let output_format_global = builder
        .build_global_string_ptr(output_format, ".output_fmt")
        .unwrap();

    // Allocate space for two integers
    let a_alloca = builder.build_alloca(i32_type, "a").unwrap();
    let b_alloca = builder.build_alloca(i32_type, "b").unwrap();

    // Call scanf to read two integers
    builder
        .build_call(
            scanf_fn,
            &[
                input_format_global.as_pointer_value().into(),
                a_alloca.into(),
                b_alloca.into(),
            ],
            "scanf_call",
        )
        .unwrap();

    // Load the values
    let a_val = builder
        .build_load(i32_type, a_alloca, "a_val")
        .unwrap()
        .into_int_value();
    let b_val = builder
        .build_load(i32_type, b_alloca, "b_val")
        .unwrap()
        .into_int_value();

    // Add the values
    let sum = builder.build_int_add(a_val, b_val, "sum").unwrap();

    // Call printf to output the result
    builder
        .build_call(
            printf_fn,
            &[output_format_global.as_pointer_value().into(), sum.into()],
            "printf_call",
        )
        .unwrap();

    // Return 0
    let zero = i32_type.const_int(0, false);
    builder.build_return(Some(&zero)).unwrap();

    module
}

// fn main() -> Result<(), Box<dyn std::error::Error>> {
//     let context = Context::create();
//     let module = generate_program(&context);
//
//     // Print the LLVM IR
//     println!("{}", module.to_string());
//
//     Ok(())
// }
