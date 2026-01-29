use rkyv::api::high::{HighDeserializer, HighSerializer, HighValidator};
use rkyv::bytecheck::CheckBytes;
use rkyv::rancor::Error;
use rkyv::ser::allocator::ArenaHandle;
use rkyv::util::AlignedVec;
use rkyv::{Archive, Deserialize, Serialize};

pub fn encode<T>(value: &T) -> Option<Vec<u8>>
where
    T: for<'a> Serialize<HighSerializer<AlignedVec, ArenaHandle<'a>, Error>>,
{
    rkyv::to_bytes::<Error>(value).ok().map(|bytes| bytes.into_vec())
}

pub fn decode<T>(bytes: &[u8]) -> Option<T>
where
    T: Archive,
    T::Archived:
        for<'a> CheckBytes<HighValidator<'a, Error>> + Deserialize<T, HighDeserializer<Error>>,
{
    decode_result::<T>(bytes).ok()
}

pub fn decode_result<T>(bytes: &[u8]) -> Result<T, Error>
where
    T: Archive,
    T::Archived:
        for<'a> CheckBytes<HighValidator<'a, Error>> + Deserialize<T, HighDeserializer<Error>>,
{
    let align = core::mem::align_of::<T::Archived>();
    let ptr = bytes.as_ptr() as usize;
    if align <= 1 || ptr % align == 0 {
        return rkyv::from_bytes::<T, Error>(bytes);
    }

    // WebSocket frames / storage bytes are not guaranteed to be aligned.
    if align <= AlignedVec::<16>::ALIGNMENT {
        let mut aligned = AlignedVec::<16>::with_capacity(bytes.len());
        aligned.extend_from_slice(bytes);
        return rkyv::from_bytes::<T, Error>(&aligned);
    }

    let mut aligned = AlignedVec::<64>::with_capacity(bytes.len());
    aligned.extend_from_slice(bytes);
    rkyv::from_bytes::<T, Error>(&aligned)
}
