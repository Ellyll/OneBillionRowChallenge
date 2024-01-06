FROM mcr.microsoft.com/dotnet/runtime:8.0 AS base
USER $APP_UID
WORKDIR /app

FROM mcr.microsoft.com/dotnet/sdk:8.0 AS build
ARG BUILD_CONFIGURATION=Release
WORKDIR /src
COPY ["OneBillionRowChallenge/OneBillionRowChallenge.fsproj", "OneBillionRowChallenge/"]
RUN dotnet restore "OneBillionRowChallenge/OneBillionRowChallenge.fsproj"
COPY . .
WORKDIR "/src/OneBillionRowChallenge"
RUN dotnet build "OneBillionRowChallenge.fsproj" -c $BUILD_CONFIGURATION -o /app/build

FROM build AS publish
ARG BUILD_CONFIGURATION=Release
RUN dotnet publish "OneBillionRowChallenge.fsproj" -c $BUILD_CONFIGURATION -o /app/publish /p:UseAppHost=false

FROM base AS final
WORKDIR /app
COPY --from=publish /app/publish .
ENTRYPOINT ["dotnet", "OneBillionRowChallenge.dll"]
