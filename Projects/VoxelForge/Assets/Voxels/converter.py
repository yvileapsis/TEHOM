from PIL import Image

# Load the tall image
input_file = input()
output_file = input()

# Open the image
image = Image.open(input_file)

# Verify dimensions
width, height = image.size
assert width == 256 and height == 65536, "Input image must be 256x65536."

# Reshape to 4096x4096
new_width = 4096
new_height = 4096

# Create a new blank image with the same mode as the input
output_image = Image.new(image.mode, (new_width, new_height))

# Populate the new image
for row in range(256):  # 256 rows
    reverse_row = 255 - row
    strip = image.crop((0, reverse_row * 256, 256, (reverse_row + 1) * 256))  # Crop a 256x256 strip
    x_offset = (row % 16) * 256  # Column index in the new image
    y_offset = (row // 16) * 256  # Row index in the new image
    output_image.paste(strip, (x_offset, y_offset))  # Paste into the new image

# Save the reshaped image
output_image.save(output_file)
print("Reshaped texture saved to", output_file)
