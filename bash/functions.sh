prepend_to_path()
{
    local dir="$1"
    if [[ -d "$dir" ]]; then
        export PATH="$dir:$PATH"
    fi
}
