cd ./files-compiled
rm -f *.o
rm -f main
rm -f *.hi

cd ..
ghc -o main Main.hs
for arquivo in *; do
  if [ -f "$arquivo" ] && echo "$arquivo" | grep -qv '\.sh$' && echo "$arquivo" | grep -qv '\.hs$'; then
    mv "$arquivo" "./files-compiled/"
  fi
done

./files-compiled/main