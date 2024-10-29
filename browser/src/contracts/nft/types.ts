import { NFTMetadataDetails } from "lucid-cardano";
import type { z } from 'zod';
import type { assetNameSchema } from './schema';

export type NFTMetadata = {
  [policyId: string]: {
    [assetName: string]: NFTMetadataDetails
  }
  //@ts-ignore: Cannot use map of strings and version as number in a type def
  version?: number // number
}

// NOTE: these are ripped from `lucid-cardano` and modified because his types don't match the CIP-25 standard!!!
export type MediaAsset = {
  name: string
  image: string | string[]
  mediaType?: string
  description?: string | string[]
  files?: MediaAssetFile[]
  [key: string]: unknown
}

declare type MediaAssetFile = {
  name?: string
  mediaType: string
  src: string | string[]
}

export type AssetName = z.infer<typeof assetNameSchema>;

export type MediaAssets = {
	[key: AssetName]: MediaAsset;
};

