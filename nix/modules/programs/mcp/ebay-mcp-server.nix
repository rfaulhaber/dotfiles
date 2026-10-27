{
  lib,
  buildNpmPackage,
  fetchFromGitHub,
}:
buildNpmPackage {
  pname = "ebay-mcp-server";
  version = "1.2.0";

  # Upstream publishes to npm but tags nothing, so this pins a commit whose
  # package.json carries the version above.
  src = fetchFromGitHub {
    owner = "eBay";
    repo = "npm-public-api-mcp";
    rev = "4baa7051a5f98c8c8a310d681f4cbec269387157";
    hash = "sha256-fHxevn911JUM7JFf5Oi5bJuA1oF54n2/6MMdtjOwM8s=";
  };

  npmDepsHash = "sha256-wsJ1dY2y0HyWj7jrYi3FUGN8F75Emz/5ya5HCKtvGjc=";

  meta = {
    description = "eBay's MCP server for read-only access to its public REST and GraphQL APIs";
    homepage = "https://github.com/eBay/npm-public-api-mcp";
    license = lib.licenses.asl20;
    mainProgram = "ebay-mcp-server";
  };
}
