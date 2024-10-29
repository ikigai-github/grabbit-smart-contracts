import { Constr } from "lucid-cardano"
import * as L from "lucid-cardano"

export const isArrayOf =
  <T>(typeGuard: (o: any) => o is T) =>
  (o: any): o is T[] => {
    return Array.isArray(o) && o.every(typeGuard)
  }

function assertUnreachable(_: never): never {
  throw new Error("Unreachable")
}

export type DataAsJSON = B | I | L | M | C

type B = { contents: string; tag: "B" }
type I = { contents: bigint; tag: "I" }
type L = { contents: DataAsJSON[]; tag: "List" }
type C = { contents: [bigint, DataAsJSON[]]; tag: "Constr" }
type M = { contents: { k: DataAsJSON; v: DataAsJSON }[]; tag: "Map" }

export type Data = BytesData | IntData | ListData | MapData | ConstructorData

type BytesData = { bytes: string }
type IntData = { int: bigint }
type ListData = { list: Data[] }
type ConstructorData = { constructor: bigint; fields: Data[] }
type MapData = { map: { k: Data; v: Data }[] }

const isBytesData = (o: any): o is BytesData => {
  return typeof o.bytes === "string"
}

const isIntData = (o: any): o is IntData => {
  return typeof o.int === "bigint"
}

const isListData = (o: any): o is ListData => {
  return isArrayOf(isData)(o.list)
}

const isKVPair = (o: any): o is { k: Data; v: Data } => {
  return isData(o.k) && isData(o.v)
}

const isMapData = (o: any): o is MapData => {
  return isArrayOf(isKVPair)(o.map)
}

const isConstructorData = (o: any): o is ConstructorData => {
  return typeof o.constructor === "bigint" && isArrayOf(isData)(o.fields)
}

export const isData = (o: any): o is Data => {
  return isBytesData(o) || isIntData(o) || isListData(o) || isMapData(o) || isConstructorData(o)
}

export type Encoder<S, T> = {
  name: string | string[]
  subEncoders?: Encoder<any, any>[]
  validator: (a: S) => T
  proxy: S | null
}

export type SchemaField = readonly [string, Encoder<any, any>]
export type Schema = {
  readonly name: string
  readonly constructor: bigint
  fields: readonly SchemaField[]
}

type ArrayKeys = keyof (readonly any[])
type Indices<T> = Exclude<keyof T, ArrayKeys>

export type SchemaValue<T> = T extends { kind: infer V } ? (V extends string ? V : never) : never

export const id = (v: any) => v
export const rawDataEncoder: Encoder<Data, Data> = {
  name: "data",
  validator: id,
  proxy: null,
}
export const bigintEncoder: Encoder<bigint, bigint> = {
  name: "bigint",
  validator: id,
  proxy: null,
}
export const stringEncoder: Encoder<string, string> = {
  name: "string",
  validator: id,
  proxy: null,
}
export const listEncoder = <T>(encoder: Encoder<T, T>): Encoder<T[], T[]> => ({
  name: "list",
  subEncoders: [encoder],
  validator: id,
  proxy: null,
})
export const validListEncoder = <S, T>(encoder: Encoder<S, T>): Encoder<S[], T[]> => ({
  name: "list",
  subEncoders: [encoder],
  validator: id,
  proxy: null,
})
export const mapEncoder = <K, V>(
  keyEncoder: Encoder<K, K>,
  valueEncoder: Encoder<V, V>
): Encoder<Map<K, V>, Map<K, V>> => ({
  name: "map",
  subEncoders: [keyEncoder, valueEncoder],
  validator: id,
  proxy: null,
})
export const validMapEncoder = <J, K, U, V>(
  keyEncoder: Encoder<J, K>,
  valueEncoder: Encoder<U, V>
): Encoder<Map<J, U>, Map<K, V>> => ({
  name: "map",
  subEncoders: [keyEncoder, valueEncoder],
  validator: id,
  proxy: null,
})
export const schemaEncoder = <T>(schemaName: SchemaValue<T>): Encoder<T, T> => ({
  name: schemaName,
  validator: id,
  proxy: null,
})

// FIXME: order of constructors in C isn't checked against constructor indices of T
export const unionEncoder = <T extends { kind: C[number] }, C extends string[]>(constructors: C): Encoder<T, T> => ({
  name: constructors,
  validator: id,
  proxy: null,
})

export type SchemaToType<S extends Schema> = { kind: S["name"] } & {
  [Index in Indices<S["fields"]> as S["fields"][Index] extends readonly [any, unknown]
    ? S["fields"][Index][0]
    : never]: S["fields"][Index] extends readonly [string, Encoder<any, any>]
    ? NonNullable<S["fields"][Index][1]["proxy"]>
    : never
}

const schemata: Map<string, Schema> = new Map<string, Schema>()
export const addTypeSchema = (schema: Schema) => {
  schemata.set(schema.name, schema)
}

export const toDataAsJSON = (data: Data): DataAsJSON => {
  if (isBytesData(data)) {
    return { contents: data.bytes, tag: "B" }
  } else if (isIntData(data)) {
    return { contents: data.int, tag: "I" }
  } else if (isListData(data)) {
    return { contents: data.list.map(toDataAsJSON), tag: "List" }
  } else if (isMapData(data)) {
    const map: { k: DataAsJSON; v: DataAsJSON }[] = []
    data.map.map(({ k, v }) => (toDataAsJSON(k), toDataAsJSON(v)))
    return { contents: map, tag: "Map" }
  } else if (isConstructorData(data)) {
    return {
      contents: [data.constructor, data.fields.map(toDataAsJSON)],
      tag: "Constr",
    }
  } else {
    assertUnreachable(data)
  }
}

const dataToPlutusData = (data: Data): L.Data => {
  if (isBytesData(data)) {
    return data.bytes
  } else if (isIntData(data)) {
    return BigInt(data.int)
  } else if (isListData(data)) {
    return data.list.map(dataToPlutusData)
  } else if (isMapData(data)) {
    const map: Map<L.Data, L.Data> = new Map()
    data.map.map(({ k, v }) => map.set(dataToPlutusData(k), dataToPlutusData(v)))
    return map
  } else if (isConstructorData(data)) {
    return new Constr(Number(data.constructor), data.fields.map(dataToPlutusData))
  } else {
    assertUnreachable(data)
  }
}

export const plutusDataToData = (data: L.Data): Data => {
  if (typeof data === "bigint") {
    return { int: data }
  } else if (typeof data === "string") {
    return { bytes: data }
  } else if (data instanceof Array) {
    return { list: data.map(plutusDataToData) }
  } else if (data instanceof Map) {
    const mapData: { k: Data; v: Data }[] = []
    data.forEach((v, k) => {
      mapData.push({ k: plutusDataToData(k), v: plutusDataToData(v) })
    })
    return { map: mapData }
  } else if (data instanceof Constr) {
    return {
      constructor: BigInt(data.index),
      fields: data.fields.map(plutusDataToData),
    }
  } else {
    throw new Error("unreachable")
  }
}

export const toLucidData = (datum: any): string => L.Data.to(toPlutusData(datum))

export const toPlutusData = (v: any): L.Data => dataToPlutusData(toData(v))

export const fromData = <S extends Schema>(schema: S, data: Data): SchemaToType<S> => {
  if (isConstructorData(data) && data.constructor === schema.constructor) {
    let index = 0
    const object: any = {
      kind: schema.name,
    }
    const decodeField = (schemaField: SchemaField, fieldData: Data): any => {
      const encoder = schemaField[1]
      switch (encoder.name) {
        case "data":
          return fieldData
        case "string":
          return (fieldData as BytesData).bytes
        case "bigint":
          return BigInt((fieldData as IntData).int)
        case "list":
          if (encoder.subEncoders === undefined) throw new Error("Unknown encoder for list items")
          return (fieldData as ListData).list.map((v, i) => decodeField([`${i}`, encoder.subEncoders![0]!], v))
        case "map":
          if (encoder.subEncoders === undefined) throw new Error("Unknown encoder for map items")
          const m: Map<any, any> = new Map()
          ;(fieldData as MapData).map.forEach(({ k, v }, i) => {
            m.set(
              decodeField([`k${i}`, encoder.subEncoders![0]!], k),
              decodeField([`v${i}`, encoder.subEncoders![1]!], v)
            )
          })
          return m
        default:
          let childSchema
          if (Array.isArray(encoder.name)) {
            if (!isConstructorData(fieldData)) throw new Error("Union value is not constructor data")
            childSchema = schemata.get(encoder.name[Number((fieldData as ConstructorData).constructor)]!)
          } else {
            childSchema = schemata.get(encoder.name)
          }
          if (childSchema === undefined) throw new Error(`Could not find schema: ${encoder.name}`)
          return fromData(childSchema, fieldData)
      }
    }

    schema.fields.forEach((schemaField) => {
      object[schemaField[0]] = decodeField(schemaField, data.fields[index]!)
      index++
    })
    return object
  } else {
    throw Error("All schema types have constructors")
  }
}

export const toData = (encodable: any, subEncoders?: Encoder<any, any>[]): Data => {
  if (isData(encodable)) {
    return encodable
  } else if (typeof encodable === "bigint") {
    return { int: encodable }
  } else if (typeof encodable === "string") {
    return { bytes: encodable }
  } else if (Array.isArray(encodable)) {
    if (subEncoders !== undefined)
      return {
        list: encodable.map((v) => toData(subEncoders[0]!.validator(v), subEncoders[0]!.subEncoders)),
      }
    else return { list: encodable.map((v) => toData(v)) }
  } else if (encodable instanceof Map) {
    const mapData: { k: Data; v: Data }[] = []
    encodable.forEach((v, k) => {
      if (subEncoders !== undefined)
        mapData.push({
          k: toData(subEncoders[0]!.validator(k), subEncoders[0]!.subEncoders),
          v: toData(subEncoders[1]!.validator(v), subEncoders[1]!.subEncoders),
        })
      else mapData.push({ k: toData(k), v: toData(v) })
    })
    return { map: mapData }
  } else if (typeof encodable === "object" && "kind" in encodable && schemata.has(encodable.kind)) {
    const schema = schemata.get(encodable.kind)
    if (schema === undefined) throw new Error(`Schema not found: ${encodable.kind}`)
    return {
      constructor: schema.constructor,
      fields: schema.fields.map((field) => {
        return toData(field[1].validator(encodable[field[0]]), field[1].subEncoders)
      }),
    }
  } else {
    assertUnreachable(encodable as never)
  }
}

export const objectToData = (object: any): Data => {
  if (isData(object)) return object
  else {
    throw Error("not Data")
  }
}

export const parseFromSchema =
  <S extends Schema>(schema: S) =>
  (o: any): SchemaToType<S> => {
    return fromData(schema, objectToData(o))
  }
