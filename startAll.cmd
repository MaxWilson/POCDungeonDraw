pushd api && start func start --csharp
popd
start npm start
start swa start http://localhost:3000 --api-location http://localhost:7071
start http://localhost:4280/