namespace Storage.Events

type IStream<'TValue> =
  abstract member ReadAll: unit -> 'TValue seq
  abstract member Append: values:'TValue list -> unit
  abstract member Clear: unit -> unit

type IStore<'TKey, 'TValue when 'TKey: comparison> =
  abstract member GetStream: 'TKey -> IStream<'TValue>
