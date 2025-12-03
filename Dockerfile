# Build stage
FROM node:20-slim AS builder
WORKDIR /app
COPY 2025/package*.json ./
RUN npm install
COPY 2025/ ./
RUN npm run build

# Serve stage
FROM nginx:alpine
COPY --from=builder /app/dist /usr/share/nginx/html
EXPOSE 80
CMD ["nginx", "-g", "daemon off;"]
