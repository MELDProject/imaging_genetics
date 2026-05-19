#!/usr/bin/env bash 

usage() {
    echo 
    echo "usage: source bids2zip.sh <input_data_gene_dir> -b <yyyymmdd>"
    echo "example: source bids2zip.sh /imagine_data_T1/input_data/TSC -b 20260523"
    return 1 2>/dev/null || exit 1
}

# arguments parsing & validation 
if [[ $# -lt 3 ]]; then
    usage
fi

INPUT_DIR="$1"
shift

BATCH_DATE=""

while [[ $# -gt 0 ]]; do

    case "$1" in

        -b)
            BATCH_DATE="$2"
            shift 2
            ;;

        -h|--help)
            usage
            ;;

        *)
            echo "ERROR: Unknown argument: $1"
            usage
            ;;
    esac

done

if [[ -z "$INPUT_DIR" || -z "$BATCH_DATE" ]]; then
    echo "ERROR: Missing required arguments"
    usage
fi

if ! [[ "$BATCH_DATE" =~ ^[0-9]{8}$ ]]; then
    echo "ERROR: Date must be YYYYMMDD"
    return 1 2>/dev/null || exit 1
fi


# paths 
INPUT_DIR=$(realpath "$INPUT_DIR")

if [[ ! -d "$INPUT_DIR" ]]; then
    echo "ERROR: Input directory not found:"
    echo "  $INPUT_DIR"
    return 1 2>/dev/null || exit 1
fi

FOLDER_NAME=$(basename "$INPUT_DIR")
GENE_SITE=$(basename "$INPUT_DIR")
GENE="${FOLDER_NAME%%_*}"
SITE="${FOLDER_NAME#*_}"

BASE_DIR=$(dirname "$(dirname "$INPUT_DIR")")
ID_LIST="${INPUT_DIR}/${GENE}_id_list_${BATCH_DATE}.txt"
BIDS_DIR="${BASE_DIR}/share_data/${GENE_SITE}/BIDS"
OUTPUT_ZIP="${BASE_DIR}/share_data/${GENE_SITE}/imagine_data_${GENE_SITE}_${BATCH_DATE}.zip"
TMP_DIR=$(mktemp -d)

cleanup() {
    rm -rf "$TMP_DIR"
}

trap cleanup EXIT

if [[ ! -f "$ID_LIST" ]]; then
    echo "ERROR: ID list not found:"
    echo "  $ID_LIST"
    return 1 2>/dev/null || exit 1
fi

if [[ ! -d "$BIDS_DIR" ]]; then
    echo "ERROR: BIDS directory not found:"
    echo "  $BIDS_DIR"
    return 1 2>/dev/null || exit 1
fi

mkdir -p "${TMP_DIR}/BIDS"

# print 
echo
echo "=================================================="
echo "Creating BIDS ZIP package"
echo "Site       : $SITE"
echo "Gene       : $GENE"
echo "Batch date : $BATCH_DATE"
echo "Input dir  : $INPUT_DIR"
echo "ID list    : $ID_LIST"
echo "BIDS dir   : $BIDS_DIR"
echo "=================================================="

# copy 
copied_count=0

while IFS= read -r subject_id || [[ -n "$subject_id" ]]; do

    subject_id=$(echo "$subject_id" | xargs)

    [[ -z "$subject_id" ]] && continue

    # allow IDs with or without sub-
    if [[ "$subject_id" =~ ^sub- ]]; then
        sub_name="$subject_id"
    else
        sub_name="sub-${subject_id}"
    fi

    src_dir="${BIDS_DIR}/${sub_name}"

    if [[ ! -d "$src_dir" ]]; then

        echo "[WARNING] Subject not found:"
        echo "          $sub_name"

        continue
    fi

    echo "[COPY] $sub_name"

    cp -r "$src_dir" "${TMP_DIR}/BIDS/"

    copied_count=$(( copied_count + 1 ))

done < "$ID_LIST"

cp "$ID_LIST" "${TMP_DIR}/"

# create zip 
echo
echo "Creating ZIP archive..."

rm -f "$OUTPUT_ZIP"

(
    cd "$TMP_DIR" || exit 1

    zip -r "$OUTPUT_ZIP" \
        "BIDS" \
        "$(basename "$ID_LIST")"
)

echo
echo "=================================================="
echo "ZIP archive created successfully"
echo "Subjects copied : $copied_count"
echo "Output ZIP      : $OUTPUT_ZIP"
echo "=================================================="
