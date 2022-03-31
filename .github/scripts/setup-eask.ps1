npm install
echo "($pwd).path/bin" | Out-File -FilePath $env:GITHUB_PATH -Encoding utf8 -Append
