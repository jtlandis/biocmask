use extendr_api::prelude::*;

/// Return string `"Hello world!"` to R.
/// @export
#[extendr]
fn hello_world() -> &'static str {
    "Hello world!"
}

/// @param x an integer array
/// @param y an integer array
/// @export
#[extendr]
fn add2_slice(x: &[i32], y: &[i32]) -> Vec<i32> {
    x.iter().enumerate().map(|(i, x)| x + y[i]).collect()
}

/// example of how to modifiy R objects
/// inplace
/// @param x an integer R object
/// @export
#[extendr]
fn mutate_first(x: Robj) {
  let mut x = x;
  use extendr_api::AsTypedSlice;
  let mut x: &mut [i32] = x.as_typed_slice_mut().unwrap();
  x[0] += 3;
}

/*
#[extendr]
fn my_slice<'a>(x: &'a [i32], ind: Robj) -> &'a [&[i32]] {
    let slice_x = x; // = x.as_integer_slice().ok_or("expected integer slice")?;
    let slice_ind = ind.as_list().ok_or("expected list");
    if let Ok(slice_ind) = slice_ind {
        for list_obj in slice_ind.iter() {
            let ind_v = list_obj
                .1
                .as_integer_slice()
                .ok_or("expected integer slice");
            if let Ok(int_vec) = ind_v {
                for int in int_vec {
                    if *int >= 0 {
                        let int = *int as usize;
                        let slice = &slice_x[int..(int + 1)];
                        return &[slice];
                    }
                }
            }
        }
    }

    panic!("problem");
}
*/

// Macro to generate exports.
// This ensures exported functions are registered with R.
// See corresponding C code in `entrypoint.c`.
extendr_module! {
    mod tidySEabstraction;
    fn hello_world;
    fn add2_slice;
    fn mutate_first;
}
