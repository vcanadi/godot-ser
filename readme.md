# godot-ser

Binary serialization and deserialization of Haskell types.
Serialization is compatible with the way Godot serializes natively (var_to_bytes, bytes_to_var)

## Description

Typeclass `Serializable` for an instance `a` defines serialization function `ser :: a -> ByteString`
and deserialization attoparsec parser `des :: Parser a`.
Deserialization function `des` returns either `DesErr` or value `a`
`Serializable` instance can be derived generically.
Serialization examples can be found in Data.Godot.SerializeSpec module.
