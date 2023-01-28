pushd api && start func start --csharp
popd
start npm start
start swa start http://127.0.0.1:3000 --api-location http://127.0.0.1:7071
start http://localhost:4280/