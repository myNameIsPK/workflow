# Test treesitter injection by filename
cat <<EOF > /tmp/test.yaml
key:
  val1: "hello"
  val2: 123
EOF

# TODO: Test treesitter injection by specify filetype
cat <<EOF # ft=c
int main() {
    printf("hello\n")
}
EOF

# TODO: Test treesitter injection by specify filetype (overwrite filename)
cat <<EOF > /tmp/test.yaml # ft=json
{
    "key": "val",
    "array": [1, 2, 3]
}
EOF
