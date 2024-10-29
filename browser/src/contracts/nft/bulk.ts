import {
	applyParamsToScript,
	Constr,
	Data,
	fromText,
	Lucid,
	TxComplete,
	type Assets,
	type MintingPolicy,
	type NFTMetadataDetails,
	type UTxO
} from 'lucid-cardano';
import type { MediaAssets } from './types';
import { bulkMintingPolicy } from '../../../../compiled/noTrace';

function createBulkMintParams(utxo: UTxO, collectionSize: bigint) {
	// newtype TxId :: TxId PlutusTx.BuiltinByteString
	const txId = new Constr(0, [utxo.txHash]);

	// data TxOutRef = TxOutRef {
	//     txOutRefId  :: TxId,
	//     txOutRefIdx :: Integer -- ^ Index into the referenced transaction's outputs
	// }
	const txOutRef = new Constr(0, [txId, BigInt(utxo.outputIndex)]);

	//  data BulkMintParameters = BulkMintParameters
	//      { uniqueRef :: TxOutRef
	//      , collectionSize :: Integer
	//      }
	return new Constr(0, [txOutRef, collectionSize]);
}

export function createBulkMintPolicy(
	utxo: UTxO,
	collectionSize: bigint
): MintingPolicy {
	const params = createBulkMintParams(utxo, collectionSize);
	const script = applyParamsToScript(bulkMintingPolicy, [params]);

	return {
		type: 'PlutusV2',
		script
	};
}

export async function createBulkMintTransaction(
	lucid: Lucid,
	metadataAssets: MediaAssets
): Promise<
	| { tx: TxComplete; error?: undefined }
	| { tx?: undefined; error: 'sanitization-failed' | 'empty-wallet' }
> {
	const sanitizedSuccessfully = sanitizeMetadataAssets(metadataAssets);
	if (!sanitizedSuccessfully) {
		return { error: 'sanitization-failed' };
	}

	const utxos = await lucid.wallet.getUtxos();

	// Grab any utxo to use to create a unique policy
	if (!utxos || !utxos.length || !utxos[0]) {
		return { error: 'empty-wallet' };
	}

	const referenceUtxo = utxos[0];

	// TODO: May need more utxos to pay for the transaction if the one found wasn't big enough

	const tokenNames = Object.keys(metadataAssets);
	const policy = createBulkMintPolicy(referenceUtxo, BigInt(tokenNames.length));
	const policyId = lucid.utils.mintingPolicyToId(policy);

	const assets: Assets = {};
	tokenNames.forEach((tokenName) => {
		assets[policyId + fromText(tokenName)] = 1n;
	});

	const tx = await lucid
		.newTx()
		.collectFrom([referenceUtxo])
		.attachMintingPolicy(policy)
		.attachMetadata(721, {
			[policyId]: metadataAssets
		})
		.mintAssets(assets, Data.void())
		.complete();

	return { tx };
}

export async function createSingleMintTransaction(
	lucid: Lucid,
	tokenName: string,
	details: NFTMetadataDetails
) {
	return createBulkMintTransaction(lucid, { [tokenName]: details });
}

function splitLongString(uri: string | string[]) {
  let result = ""
  if (uri instanceof Array) {
    result = uri.join("")
  } else {
    result = uri
  }

  if (result.length <= 64) {
    return result
  }

  const arrayLength = Math.ceil(result.length / 64)
  const splitUri = new Array<string>(arrayLength)
  for (let i = 0; i < arrayLength; i++) {
    splitUri[i] = result.substring(i * 64, (i + 1) * 64)
  }

  return splitUri
}

/**
 *
 * @param assets Assets to sanitize. This var will be modified
 * @returns If it sanitized without any issues
 */
function sanitizeMetadataAssets(assets: MediaAssets): boolean {
	// Sanitize Description, Src, and then for files
	for (const key in assets) {
		const asset = assets[key];
		if (!asset || !('image' in asset)) {
			return false;
		}

		asset.image = splitLongString(asset.image);
		if (asset.description) {
			asset.description = splitLongString(asset.description);
		}

		if (!asset.files) {
			continue;
		}

		for (let i = 0; i < asset.files.length; i++) {
			const file = asset.files[i];
			if (!file) {
				// If there are gaps in the file array, then something is wrong with the input
				return false;
			}
			file.src = splitLongString(file.src);
		}
	}

	return true;
}
