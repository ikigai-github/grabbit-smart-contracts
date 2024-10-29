import { z } from 'zod';

export const mintImageTypes = [
	'image/png',
	'image/jpeg',
	'image/jpg',
	'image/gif',
	'image/webp',
	'image/svg+xml'
];

export const MAX_FILE_SIZE = 10_000_000;

export const METADATA_BYTE_LIMIT = 64;
/**
 * This is based on the idea that an ASCII character in UTF8 will be 2 hex-chars wide.
 * https://cips.cardano.org/cips/cip25/
 *
 * i.e. 'a' translates to \x61
 *
 * NOTE: lucid doesn't appear to actually hex-encode the metadata when minting though, and
 * does it using the V1 method which just puts the UTF8 string directly in the metadata. So
 * not really sure what the point is, but it does get enforced.
 */
export const LUCID_CHAR_LIMIT = 32;

// This regex pattern matches 0 or more ascii characters, excluding:
// - control characters (ascii codes 0-31)
// - DEL (ascii code 127)
const asciiRegexPattern = /^[\x20-\x7E]*$/;
const asciiOnlyRefinement = (value: string) => asciiRegexPattern.test(value);

/**
 *  This is the "on-chain" name of the NFT. It can only contain alphanumeric characters.
 */
export const tokenNameSchema = z
	.string({ required_error: 'Your NFT must have an on-chain name' })
	.min(1, { message: 'Your NFT must have an on-chain name' })
	.max(LUCID_CHAR_LIMIT, {
		message: `Your on-chain must be ${LUCID_CHAR_LIMIT} characters or less`
	})
	.refine((value) => /^[a-zA-Z0-9]*$/.test(value), {
		message:
			'The on-chain name is limited to alphanumeric characters (a-z, A-Z, 0-9)'
	});

/**
 *   This is the "pretty" name of the NFT. It can contain any ascii characters.
 * */
export const assetNameSchema = z
	.string({ required_error: 'The NFT must have a title' })
	.min(1, { message: 'The NFT must have a title' })
	.max(LUCID_CHAR_LIMIT, {
		message: `The title must be ${LUCID_CHAR_LIMIT} characters or less`
	})
	.refine(asciiOnlyRefinement, {
		message: 'The title may only contain human-readable ASCII characters'
	});
export type AssetName = z.infer<typeof assetNameSchema>;
/**
 * This is the description of the NFT. It can contain up to 1024 human-readable ascii characters.
 */
export const descriptionSchema = z
	.string()
	.max(1024, { message: 'Your description must be 1024 characters or less' })
	.refine(asciiOnlyRefinement, {
		message: 'The description may only contain human-readable ASCII characters'
	})
	.optional();
export type Description = z.infer<typeof descriptionSchema>;

export const imageSchema = z
	.instanceof(Array<Blob>, {
		message: 'You must provide an image for the NFT.'
	})
	.superRefine((files, ctx) => {
		if (files.length < 1 || !files[0]) {
			ctx.addIssue({
				code: z.ZodIssueCode.custom,
				message: 'An image is required to mint an NFT.'
			});
			return;
		}
		const file = files[0];
		if (file.size > MAX_FILE_SIZE) {
			ctx.addIssue({
				code: z.ZodIssueCode.custom,
				message: `Your image must be less than ${MAX_FILE_SIZE / 1_000_000}MB`
			});
		}
		if (!mintImageTypes.includes(file.type)) {
			ctx.addIssue({
				code: z.ZodIssueCode.custom,
				message: `The image must be a PNG, JPEG, GIF, WEBP, or SVG file. You selected a '${file.type}' file.`
			});
		}
	});
export const fileSchema = z
	.instanceof(Array<Blob>)
	.nullish()
	.superRefine((files, ctx) => {
		if (files && files.length > 0) {
			for (let i = 0; i < files.length; i++) {
				const file = files[i] as File;
				if (file && file.size > MAX_FILE_SIZE) {
					ctx.addIssue({
						code: z.ZodIssueCode.custom,
						message: `File '${file.name}' exceeds the 10MB size limit`
					});
				}
			}
		}
	});
export const bulkMintTokenFormSchema = z.object({
	tokenName: tokenNameSchema,
	assetName: assetNameSchema,
	description: descriptionSchema,
	image: imageSchema,
	files: fileSchema
});

export const editCollectionNFtSchema = z.object({
	title: assetNameSchema,
	description: descriptionSchema,
	tokenName: tokenNameSchema
});
export type EditCollectionNft = z.infer<typeof editCollectionNFtSchema>;

export const mintDatabaseEntrySchema = z.object({
	asset_id: z.string(),
	token_name: z.string().min(1).max(METADATA_BYTE_LIMIT),
	currency_symbol: z.string().min(1),
	title: z.string().min(1).max(METADATA_BYTE_LIMIT),
	description: z.string().min(1).max(1024),
	owner_wallet: z.string().min(1), //hexAddressSchema in Apollo
	creator_wallet: z.string().min(1), //hexAddressSchema in Apollo
	preview_cdn: z.string().url(),
	ipfs_cid: z.string().min(1),
	created: z.date()
});
export type MintDatabaseEntry = z.infer<typeof mintDatabaseEntrySchema>;