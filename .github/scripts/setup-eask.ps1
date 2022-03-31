npm install
echo "${{ github.workspace }}/bin" | Out-File -FilePath $env:GITHUB_PATH -Encoding utf8 -Append
