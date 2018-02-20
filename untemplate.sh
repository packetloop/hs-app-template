#!/bin/bash

PRJ_NAME=$(basename $PWD)

if [[ "$PRJ_NAME" == *"template"* ]]; then
  echo "This is a template project."
  echo "Please copy it before un-templating."
  exit 1
fi

echo "Un-templating into $PRJ_NAME"
rm -rf .git .stack-work
find . -type f -exec sed -i '' "s/hs-app-template/$PRJ_NAME/g" {} +

cat > ./README.md <<EOF
# $PRJ_NAME

EOF

rm untemplate.sh

git init
git add .
git commit -m "Cloned from a template"
