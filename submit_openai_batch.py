import os
from openai import OpenAI
from dotenv import load_dotenv

load_dotenv()

# Get API key from environment variable
api_key = os.getenv("OPENAI_API_KEY")
if not api_key:
    raise RuntimeError("OPENAI_API_KEY environment variable not set. Please set it in your .env file or environment.")

client = OpenAI(api_key=api_key)

BATCH_INPUT_DIR = os.path.join("..", "Data", "LoansFull", "loan_extractions")
BATCH_INPUT_FILES = [
    os.path.join(BATCH_INPUT_DIR, f"batch_input_part{i}.jsonl") for i in range(1, 6)
]

def upload_batch_file(batch_file_path):
    print(f"Uploading batch input file: {batch_file_path} ...")
    with open(batch_file_path, "rb") as f:
        batch_input_file = client.files.create(
            file=f,
            purpose="batch"
        )
    print(f"Uploaded file ID: {batch_input_file.id}")
    return batch_input_file.id

def create_batch(file_id, part_num):
    print(f"Creating batch job for part {part_num}...")
    batch = client.batches.create(
        input_file_id=file_id,
        endpoint="/v1/chat/completions",
        completion_window="24h",
        metadata={"description": f"loan officer extraction batch part {part_num}"}
    )
    print(f"Batch ID for part {part_num}: {batch.id}")
    print("Save this Batch ID to download results later.")
    return batch.id

if __name__ == "__main__":
    for idx, batch_file in enumerate(BATCH_INPUT_FILES, start=1):
        if not os.path.exists(batch_file):
            print(f"Batch input file not found: {batch_file}")
            continue
        file_id = upload_batch_file(batch_file)
        batch_id = create_batch(file_id, idx)