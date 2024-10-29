import { Address, Assets, Data, getAddressDetails, Lucid, UTxO } from "lucid-cardano";

function union(
    a1: Assets,
    a2: Assets
) {
    const a2Entries = Object.entries(a2);

    // initialize with clone of a1
    const result: Assets = { ...a1 };

    // add or append entries from a2
    a2Entries.forEach(([key, quantity]) => {
        if (result[key]) {
            result[key] += quantity;
        } else {
            result[key] = quantity;
        }
    });

    return result;
}

export const PolicyId = Data.Bytes({ minLength: 28, maxLength: 28 });

export const Value = Data.Map(
    PolicyId,
    Data.Map(Data.Bytes(), Data.Integer()),
);
export type Value = Data.Static<typeof Value>;

function fromAssets(assets: Assets): Value {
    const value = new Map<string, Map<string, bigint>>();
    if (assets["lovelace"]) value.set("", new Map([["", assets["lovelace"]]]));

    const units = Object.keys(assets);
    const policies = Array.from(
        new Set(
            units
                .filter((unit) => unit !== "lovelace")
                .map((unit) => unit.slice(0, 56)),
        ),
    );
    policies.sort().forEach((policyId) => {
        const policyUnits = units.filter((unit) => unit.slice(0, 56) === policyId);
        const assetsMap = new Map<string, bigint>();
        policyUnits.sort().forEach((unit) => {
            assetsMap.set(
                unit.slice(56),
                assets[unit]!,
            );
        });
        value.set(policyId, assetsMap);
    });
    return value;
}

function toAssets(value: Value): Assets {
    const result: Assets = { lovelace: value.get("")?.get("") || BigInt(0) };

    for (const [policyId, assets] of value) {
        if (policyId === "") continue;
        for (const [assetName, amount] of assets) {
            result[policyId + assetName] = amount;
        }
    }
    return result;
}

export {
    union,
    fromAssets,
    toAssets,
};

export const PCredential = Data.Enum([
    Data.Object({
        PublicKeyCredential: Data.Tuple([
            Data.Bytes({ minLength: 28, maxLength: 28 }),
        ]),
    }),
    Data.Object({
        ScriptCredential: Data.Tuple([
            Data.Bytes({ minLength: 28, maxLength: 28 }),
        ]),
    }),
]);
export type PCredential = Data.Static<typeof PCredential>;

export const PAddress = Data.Object({
    paymentCredential: PCredential,
    stakeCredential: Data.Nullable(Data.Enum([
        Data.Object({ Inline: Data.Tuple([PCredential]) }),
        Data.Object({
            Pointer: Data.Tuple([Data.Object({
                slotNumber: Data.Integer(),
                transactionIndex: Data.Integer(),
                certificateIndex: Data.Integer(),
            })]),
        }),
    ])),
});
export type PAddress = Data.Static<typeof PAddress>;

export function fromAddress(address: Address): PAddress {
    // We do not support pointer addresses!

    const { paymentCredential, stakeCredential } = getAddressDetails(
        address,
    );

    if (!paymentCredential) throw new Error("Not a valid payment address.");

    return {
        paymentCredential: paymentCredential?.type === "Key"
            ? {
                PublicKeyCredential: [paymentCredential.hash],
            }
            : { ScriptCredential: [paymentCredential.hash] },
        stakeCredential: stakeCredential
            ? {
                Inline: [
                    stakeCredential.type === "Key"
                        ? {
                            PublicKeyCredential: [stakeCredential.hash],
                        }
                        : { ScriptCredential: [stakeCredential.hash] },
                ],
            }
            : null,
    };
}

export function toAddress(address: PAddress, lucid: Lucid): Address {
    const paymentCredential = (() => {
        if ("PublicKeyCredential" in address.paymentCredential) {
            return lucid.utils.keyHashToCredential(
                address.paymentCredential.PublicKeyCredential[0],
            );
        } else {
            return lucid.utils.scriptHashToCredential(
                address.paymentCredential.ScriptCredential[0],
            );
        }
    })();
    const stakeCredential = (() => {
        if (!address.stakeCredential) return undefined;
        if ("Inline" in address.stakeCredential) {
            if ("PublicKeyCredential" in address.stakeCredential.Inline[0]) {
                return lucid.utils.keyHashToCredential(
                    address.stakeCredential.Inline[0].PublicKeyCredential[0],
                );
            } else {
                return lucid.utils.scriptHashToCredential(
                    address.stakeCredential.Inline[0].ScriptCredential[0],
                );
            }
        } else {
            return undefined;
        }
    })();
    return lucid.utils.credentialToAddress(paymentCredential, stakeCredential);
}

export const OfferDatum = Data.Object({
    creator: PAddress,
    toBuy: Value
});
export type OfferDatum = Data.Static<typeof OfferDatum>;

export type OfferInfo = {
    creator: Address,
    toBuy: Value,
    offer: Value
    offerUTxO: UTxO
};

export const RoyaltyRecipient = Data.Object({
    address: PAddress,
    fee: Data.Integer({ minimum: 1 }),
    minFee: Data.Nullable(Data.Integer()),
    maxFee: Data.Nullable(Data.Integer()),
});
export type RoyaltyRecipient = Data.Static<typeof RoyaltyRecipient>;

export const RoyaltyInfo = Data.Object({
    recipients: Data.Array(RoyaltyRecipient),
    version: Data.Integer({ minimum: 1, maximum: 1 }),
    extra: Data.Any(),
});
export type RoyaltyInfo = Data.Static<typeof RoyaltyInfo>;

export const Metadata333 = Data.Map(Data.Bytes(), Data.Any());
export type Metadata333 = Data.Static<typeof Metadata333>;

export const Metadata222 = Data.Map(Data.Bytes(), Data.Any());
export type Metadata222 = Data.Static<typeof Metadata222>;

export const DatumNFTMetadata = Data.Object({
    metadata: Metadata222,
    version: Data.Integer({ minimum: 1, maximum: 1 }),
    extra: Data.Any(),
});
export type DatumNFTMetadata = Data.Static<typeof DatumNFTMetadata>;

export const EvolveInfo = Data.Object({
    stage:  Data.Integer({ minimum: 0}),
    stages: Data.Integer({ minimum: 1}),
    price: Value, 
    recipient: PAddress,
    isPaid: Data.Boolean()
  });
export type EvolveInfo = Data.Static<typeof EvolveInfo>;

export const MetadataEvolveDatum = Data.Object({
    metadata: Metadata222,
    version: Data.Integer({ minimum: 1, maximum: 1 }),
    extra: EvolveInfo,
  });
export type MetadataEvolveDatum = Data.Static<typeof MetadataEvolveDatum>;


export type Metadata = {
    name: string;
    description: string;
    ticker?: string;
    url?: string;
    logo?: string;
    decimals?: number;
};

export type NFTMetadata = {
    name: string;
    image: string;
    mediaType?: string;
    description?: string;
    files?: FileDetails[];
};

export type FileDetails = {
    name?: string;
    mediaType: string;
    src: string;
};
