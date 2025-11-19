#!/bin/bash
# Test Habu execution in Docker (Linux environment)

set -e

echo "========================================="
echo "  Testing Habu in Docker (Linux)"
echo "========================================="
echo ""

# Create Dockerfile if it doesn't exist
cat > Dockerfile.test <<'EOF'
FROM ubuntu:22.04

# Install SBCL
RUN apt-get update && \
    apt-get install -y sbcl && \
    rm -rf /var/lib/apt/lists/*

WORKDIR /habu

# Copy project files
COPY . .

# Run tests
CMD ["sbcl", "--script", "bootstrap/test-executor.lisp"]
EOF

echo "Building Docker image..."
docker build -f Dockerfile.test -t habu-test .

echo ""
echo "Running tests in Linux container..."
echo "========================================="
echo ""

docker run --rm habu-test

echo ""
echo "========================================="
echo "  If you see errors above, Docker"
echo "  doesn't have permissions for"
echo "  executable memory either."
echo ""
echo "  Try running Docker with:"
echo "  docker run --rm --cap-add=SYS_ADMIN habu-test"
echo "========================================="
