#!/bin/bash

data_dir="$1"
shift

while getopts "b:" flag; do
    case "${flag}" in
        b) batch="${OPTARG}" ;;
    esac
done
echo "Processing batch: ${batch}"
echo "${data_dir}"

input_dir="${data_dir}/DICOM"
output_dir="${data_dir}/NIFTI"

mkdir -p "${output_dir}"
log_file="${output_dir}/dcm2niix_log.txt"
echo "Started at $(date)" > "${log_file}"

gene_site=$(basename "${data_dir}")
gene="${gene_site%%_*}"
echo "gene_site: ${gene_site}"
echo "gene: ${gene}"

txt_file="${data_dir}/${gene}_id_list_${batch}.txt"
echo "${txt_file}"
mapfile -t batch_list < "${txt_file}"

for sub in "${batch_list[@]}"; do
    subname="sub-${sub}"
    echo "${subname} : START" >> "${log_file}"    

    nifti_dir="${output_dir}/${subname}"
    dicom_dir="${input_dir}/${sub}"

    mkdir -p "${nifti_dir}" 
    echo "Converting ${subname}"

    dcm2niix -b y -ba y -z y -o "${nifti_dir}" "${dicom_dir}" 2>> "${log_file}"
    
    echo "${subname} : SUCCESS" >> "${log_file}"
done

count_inputs="${#batch_list[@]}"

count_niftis=$(find "${output_dir}" -mindepth 1 -maxdepth 1 -type d -exec sh -c '
    find "$1" -type f -name "*.nii.gz" | grep -q .
' sh {} \; -print | wc -l)

echo "--------------------------------"
echo "${count_niftis} out of ${count_inputs} subjects processed successfully"
echo "Nifti outputs are saved in: ${output_dir}"
echo "--------------------------------"

echo "${count_niftis} out of ${count_inputs} subjects processed successfully" >> "${log_file}"
echo "Nifti outputs are saved in: ${output_dir}" >> "${log_file}"
echo "Completed at $(date)" >> "${log_file}"
