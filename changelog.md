## [0.1.0] - TODO: release Date

### Added

- support for Union Types: `type instance KIND <type> = UNION`
- add `Interpreter` class with instances:

  - `ByteString`
  - `Text`
  - Lazy `ByteString`,
  - Lazy `Text`

- support of Parsing input values: `Objects`,`Arrays`
- support scalar type: `ID`
- scalar Types are validated by `GQLScalar` instance function `parseValue
- TypeFamily `KIND` with:

  - `SCALAR`
  - `OBJECT`,
  - `ENUM`
  - `INPUT_OBJECT`
  - `UNION`

- `::->` Resolver is Monad

- inline Fragments
- GraphQL [Aliases](https://graphql.org/learn/queries/#aliases)
- Subscriptions:  `GQLSubscription`
    - `::->>` operator:  is like  `::->`, wraps result inside `WithEffect`.
        is used for Mutation  and Subscribe communication
   - `withEffect ["CHANNEL_ID"] value`: packs result in `WithEffect`.
   if mutation and subscription resolver have same channel then
       every call of mutation will trigger subscription resolver
   - `GQLState`: shared  state between `http` and `websocket` server
   - `gqlSocketApp` :converts  `interpreter` to `websocket` application

### Changed
-  `GQLRoot`, `GQLType(..)` , `GQLScalar(..)` , `GQLMutation` , `GQLQuery`  , `GQLArgs`
    are moved in `Data.Morpheus.Types`
- `::->` is now type synonym fo Resolver QUERY a b  type Resolver
- `interpreter`: can be used in `http` and `websocket` server
- `GQLKind` renamed as `GQLType`
- types can be derived just with `(Generic,GQLType)`
- public API (all other modules are hidden):
  - Data.Morpheus
  - Data.Morpheus.Kind
  - Data.Morpheus.Types
  - Data.Morpheus.Server

### Fixed:

- parser can read fields with digits like: a1 , \_1
- introspection:
  - argument supports `Non-Null` and `List`
  - every field has correct kind

### Removed

- `GQLObject`: replaced with instance `type instance KIND <Type> = OBJECT`
- `GQLEnum`: replaced with instance `type instance KIND <Type> = ENUM`
- `GQLInput`: replaced with instance `type instance KIND <Type> = INPUT_OBJECT`
- `Typeable` : with new deriving it is not required anymore
- `Wrapper`: with TypeFamilies there is no need for `Wrapper`
