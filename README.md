# POCDungeonDraw

Public URL: https://wonderful-mud-00aeae01e.2.azurestaticapps.net/

Proof of concept for dungeon generator (for Shining Sword). Design-time drawing and gametime exploration, WebRTC support.

    dotnet tool restore
    dotnet paket install
    npm install
    npm install -g @azure/static-web-apps-cli azure-functions-core-tools@3

To On three separate command lines, start Fable, the az function, and swa emulator which ties them both together.
    npm start
    cd api && func start --csharp         
    swa start http://localhost:3000 --api-location http://localhost:7071
    
    REM (Yes, that --csharp is deliberate, and func will still load the .fsproj correctly.)

Then navigate to http://localhost:4280 to use the app. Both fable and swa support hot-reloading so everything should just work.

See https://github.com/aaronpowell/swa-feliz-template for more build and deployment instructions.

