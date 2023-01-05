//! General struct and traits for cyclic indexing
//!
//! # Basic Example
//! ```
//! use cyclic_idx::Cyclic;
//! let ex: Cyclic<Vec<u8>, usize, usize> = Cyclic::new(vec![1, 2, 3]);
//!
//! assert_eq!(1, ex[0]);
//! assert_eq!(1, ex[3]);
//! ```
//! # Equality
//!
//! `PartialEq` and `Eq` are defined so that two `Cyclic` structs that are "rotated" are equal:
//!
//! ```
//! # use cyclic_idx::Cyclic;
//! let ex1: Cyclic<_, usize, usize> = Cyclic::new(vec![1, 2, 3]);
//! let ex2 = Cyclic::new(vec![2, 3, 1]);
//! let ex3 = Cyclic::new(vec![3, 1, 2]);
//!
//! assert_eq!(ex1, ex2);
//! assert_eq!(ex1, ex3);
//! assert_eq!(ex2, ex3);
//! ```
//! Note that we were able to omit some type annotations because of comparison.
//!
//! Also note that this has a very limited form of equality that might not suit your
//! purposes. For instance, length of the underlying iterators must be equal, so `Cyclic::new([1, 2])` and
//! `Cyclic::new([1, 2, 1, 2])` would be treated as distinct.
//!
//! There is also no consideration given to the path through the underlying iterator imposed by how
//! indicies wrap through the data.
//!
//! # IndexMut
//!
//! You can also use cyclic indexing in a mutable context:
//!
//! ```
//! # use cyclic_idx::Cyclic;
//! let mut ex1: Cyclic<_, usize, usize> = Cyclic::new(vec![2, 3, 999]);
//! let ex2 = Cyclic::new(vec![1, 2, 3]);
//! ex1[2] = 1;
//! assert_eq!(ex1, ex2);
//! ```
//!
//! # Type bounds
//!
//! In the above examples, the struct `Cyclic` is specified with three generic arguments: the
//! original data `Vec<u8>`, the type of this data's indices `usize`, and the type we will use
//! to access these indicies `usize`.
//!
//! In the trait bounds, these two types of indicies are named `NativeIdx` and `ForeignIdx`. In
//! general, they do not need to be the same type, so long as `ForeignIdx: TryInto<native_idx>` is
//! satisfied. For example:
//!
//! ```
//! # use cyclic_idx::Cyclic;
//! let ex: Cyclic<_, usize, isize> = Cyclic::new(vec![1, 2, 3]);
//!
//! assert_eq!(1, ex[-3 as isize]);
//! assert_eq!(1, ex[0 as isize]);
//! assert_eq!(1, ex[3 as isize]);
//! ```
//!
//! # `Wrapping` trait
//!
//! The way that indices wrap is determined by their implementation of the trait [`Wrapping`](crate::Wrapping). In the above
//! examples we have seen the two implementations provided by this crate, where `NativeIdx` is
//! usize and `ForeignIdx` is either `usize` or `isize`.
//!
//! You may also use a data structure with different indexing by providing your own
//! implementation. Here is a bit of a contrived example where our indices are an enum with two
//! variants that wrap to each other:
//!
//! ```
//! use cyclic_idx::{Cyclic,Wrapping};
//! use std::ops::Index;
//!
//! enum EnumIdx {
//!     A,
//!     B,
//! }
//!
//! impl From<EnumIdx> for usize {
//!     fn from(e: EnumIdx) -> Self {
//!         match e {
//!             EnumIdx::A => 0,
//!             EnumIdx::B => 1,
//!         }
//!     }
//! }
//!
//! impl<T> Wrapping<T> for EnumIdx
//! where
//!     T: Index<usize> + IntoIterator + Clone,
//! {
//!     fn wrap(self, idx: T) -> Self {
//!         match self {
//!             EnumIdx::A => EnumIdx::B,
//!             EnumIdx::B => EnumIdx::A,
//!         }
//!     }
//! }
//!
//! let ex: Cyclic<Vec<&str>, usize, EnumIdx> = Cyclic::new(vec!["A", "B"]);
//!
//! assert_eq!("A", ex[EnumIdx::B]);
//! assert_eq!("B", ex[EnumIdx::A]);
//! ```
//! # From Generic Types
//!
//! Note that we are not just limited to using vectors as our input data. Any values that meet
//! `Index<usize> + IntoIterator + Clone` will be accepted. As an example, consider a generic
//! struct:
//!
//! ```
//! # use std::ops::Index;
//! #[derive(Clone, Debug)]
//! struct Container<T: Index<usize> + IntoIterator + Clone>(T);
//! ```
//!
//! For `Container` itself to meet our trait bounds, we need to implement the `Index` and `IntoIterator` traits:
//!
//! ```
//! # use cyclic_idx::Cyclic;
//! # use std::ops::Index;
//! # #[derive(Clone, Debug)]
//! # struct Container<T: Index<usize> + IntoIterator + Clone>(T);
//! impl<T, N> IntoIterator for Container<T>
//! where
//!     T: Index<usize, Output = N> + IntoIterator<Item = N> + Clone,
//! {
//!     type Item = N;
//!     type IntoIter = <T as IntoIterator>::IntoIter;
//!
//!     fn into_iter(self) -> Self::IntoIter {
//!         self.0.into_iter()
//!     }
//! }
//!
//! impl<T, N> Index<usize> for Container<T>
//! where
//!     T: Index<usize, Output = N> + IntoIterator<Item = N> + Clone,
//! {
//!     type Output = N;
//!
//!     fn index(&self, idx: usize) -> &N {
//!         &self.0[idx]
//!     }
//! }
//!
//! let ex: Cyclic<Container<Vec<u8>>, usize, usize> = Cyclic::new(Container(vec![1, 2, 3]));
//! assert_eq!(1, ex[0]);
//! assert_eq!(1, ex[3]);
//! ```
//!
//! Note that we have not implemented a custom method of wrapping, as we are still using usize
//! for both `NativeIdx` and `ForeignIdx`.
//!
//! # Non-`usize` NativeIdx
//!
//! Note that it is not a requirement that NativeIdx is `usize` as in our previous examples.
//! Consider the following where we define a type whose indices are a subset of isize:
//!
//! ```
//! # use cyclic_idx::{Cyclic,Wrapping};
//! # use std::ops::Index;
//! #[derive(Clone, Debug)]
//! struct IsizeSubset {
//!     non_negative: Vec<i8>,
//!     negative: Vec<i8>,
//! }
//!
//! impl IntoIterator for IsizeSubset {
//!     type Item = i8;
//!     type IntoIter = std::vec::IntoIter<Self::Item>;
//!
//!     fn into_iter(self) -> Self::IntoIter {
//!         let neg_iter = self
//!             .negative
//!             .into_iter()
//!             .rev()
//!             .into_iter();
//!         let non_neg_iter = self.non_negative.into_iter();
//!         neg_iter
//!             .chain(non_neg_iter)
//!             .collect::<Vec<i8>>()
//!             .into_iter()
//!     }
//! }
//!
//!
//! let values = IsizeSubset {
//!     non_negative: vec![0, 1, 2, 3],
//!     negative: vec![-1, -2, -3],
//! };
//!
//! assert_eq!(
//!     vec![-3, -2, -1, 0, 1, 2, 3],
//!     values.clone().into_iter().collect::<Vec<i8>>()
//! );
//!
//! impl Index<isize> for IsizeSubset {
//!     type Output = i8;
//!
//!     fn index(&self, idx: isize) -> &i8 {
//!         if idx >= 0 {
//!             &self.non_negative[idx as usize]
//!         } else {
//!             &self.negative[(-idx - 1) as usize]
//!         }
//!     }
//! }
//!
//! impl Wrapping<IsizeSubset> for isize {
//!     fn wrap(self, values: IsizeSubset) -> isize {
//!         let len_non_negative = values.non_negative.iter().len() as isize;
//!         let len_negative = values.negative.iter().len() as isize;
//!         ((self + len_negative).rem_euclid(len_non_negative + len_negative)) - len_negative
//!     }
//! }
//!
//! let ex: Cyclic<IsizeSubset, isize, isize> = Cyclic::new(values);
//!
//! assert_eq!(0, ex[0]);
//! assert_eq!(-3, ex[4]);
//! assert_eq!(-3, ex[11]);
//! assert_eq!(3, ex[-4]);
//! assert_eq!(3, ex[-11]);
//! ```

use core::fmt::Debug;
use std::fmt;
use std::marker::PhantomData;
use std::ops::{Index, IndexMut};

/// struct representing a type with cyclic indexing

pub struct Cyclic<T, NativeIdx, ForeignIdx> {
    pub values: T,
    native_idx: PhantomData<NativeIdx>,
    foreign_idx: PhantomData<ForeignIdx>,
}

impl<T, NativeIdx, ForeignIdx> Cyclic<T, NativeIdx, ForeignIdx>
where
    T: Index<NativeIdx> + IntoIterator + Clone,
    ForeignIdx: TryInto<NativeIdx> + Wrapping<T>,
{
    pub fn new(values: T) -> Self {
        Self {
            values,
            native_idx: PhantomData,
            foreign_idx: PhantomData,
        }
    }
}

impl<T: std::fmt::Debug, NativeIdx, ForeignIdx> fmt::Debug for Cyclic<T, NativeIdx, ForeignIdx> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Cyclic")
            .field("values", &self.values)
            .field("native_idx", &std::any::type_name::<NativeIdx>())
            .field("foreign_idx", &std::any::type_name::<ForeignIdx>())
            .finish()
    }
}

/// trait representing how to wrap indicies of a given type

pub trait Wrapping<T> {
    fn wrap(self, values: T) -> Self;
}

impl<T> Wrapping<T> for usize
where
    T: Index<usize> + IntoIterator + Clone,
{
    fn wrap(self, values: T) -> Self {
        let len = values.into_iter().count();
        self % len
    }
}

impl<T> Wrapping<T> for isize
where
    T: Index<usize> + IntoIterator + Clone,
{
    fn wrap(self, values: T) -> Self {
        let len = values.into_iter().count();
        self.rem_euclid(len as isize)
    }
}

// implementation of indexing

impl<T, NativeIdx, ForeignIdx, N> Index<ForeignIdx> for Cyclic<T, NativeIdx, ForeignIdx>
where
    T: Index<NativeIdx, Output = N> + IntoIterator<Item = N> + Clone,
    ForeignIdx: TryInto<NativeIdx> + Wrapping<T>,
    <ForeignIdx as TryInto<NativeIdx>>::Error: Debug,
{
    type Output = N;

    fn index(&self, idx: ForeignIdx) -> &N {
        let wrap_idx = idx.wrap(self.values.clone());
        let convert_idx = ForeignIdx::try_into(wrap_idx).unwrap();
        &self.values[convert_idx]
    }
}

// implementation of mutable indexing

impl<T, NativeIdx, ForeignIdx, N> IndexMut<ForeignIdx> for Cyclic<T, NativeIdx, ForeignIdx>
where
    T: IndexMut<NativeIdx, Output = N> + IntoIterator<Item = N> + Clone,
    ForeignIdx: TryInto<NativeIdx> + Wrapping<T>,
    <ForeignIdx as TryInto<NativeIdx>>::Error: Debug,
{
    fn index_mut(&mut self, idx: ForeignIdx) -> &mut N {
        let wrap_idx = idx.wrap(self.values.clone());
        let convert_idx = ForeignIdx::try_into(wrap_idx).unwrap();
        &mut self.values[convert_idx]
    }
}

impl<T, NativeIdx, ForeignIdx> PartialEq for Cyclic<T, NativeIdx, ForeignIdx>
where
    T: IntoIterator + Clone,
    <T as IntoIterator>::IntoIter: Clone,
    <T as IntoIterator>::Item: PartialEq,
{
    fn eq(&self, other: &Self) -> bool {
        let l_len = &self.values.clone().into_iter().count();
        let r_len = &other.values.clone().into_iter().count();

        if l_len != r_len {
            false
        } else {
            // we keep the left in place, and rotate the right
            (0..*r_len).any(|offset| {
                let mut l_iter = self.values.clone().into_iter().cycle();
                let mut r_iter = other.values.clone().into_iter().cycle().skip(offset);
                (0..*r_len).all(|_| l_iter.next().unwrap() == r_iter.next().unwrap())
            })
        }
    }
}

impl<T, NativeIdx, ForeignIdx> Eq for Cyclic<T, NativeIdx, ForeignIdx>
where
    T: IntoIterator + Clone,
    <T as IntoIterator>::IntoIter: Clone,
    <T as IntoIterator>::Item: PartialEq,
{
}
