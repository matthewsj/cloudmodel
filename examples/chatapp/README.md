Example is visible [here](https://sheltered-savannah-70764.herokuapp.com/).

### Deploying
Run the following from the root of the directory.
```sh
git push HEROKU_GIT_REMOTE_NAME `git subtree split --prefix examples/chatapp master`:master --force
```

If you want to push a branch, rather than master, you can change the `master` inside of the subtree command to the branch name you want to push.
