// SPDX-License-Identifier: PMPL-1.0-or-later
// Copyright (c) 2026 Jonathan D.A. Jewell (hyperpolymath) <j.d.a.jewell@open.ac.uk>
//
//! Gossamer Groove endpoint for vext.
//!
//! Exposes vext's NNTP/Usenet bridge capabilities via the groove discovery
//! protocol. Any groove-aware system (Gossamer, PanLL, Burble, etc.) can
//! discover vext by probing `GET /.well-known/groove` on port 6480.
//!
//! Vext is an NNTP/Usenet bridge with hash chain attestation and
//! multi-backend support. The groove connector types are formally verified
//! in Gossamer's Groove.idr:
//! - `IsSubset` proves consumers can only connect if vext satisfies their
//!   needs
//! - `GrooveHandle` is linear: consumers MUST disconnect (no dangling
//!   grooves)
//!
//! ## Groove Protocol
//!
//! - `GET  /.well-known/groove` — Capability manifest (JSON)
//! - `GET  /health`             — Simple health check
//!
//! ## Capabilities Offered
//!
//! - `nntp-bridge` — NNTP/Usenet bridge with hash chain attestation and
//!   multi-backend support
//!
//! ## Capabilities Consumed (enhanced when available)
//!
//! - `octad-storage` (from VeriSimDB) — Persist article metadata as octad
//!   entities
//! - `voice-comms` (from Burble) — Voice notification of high-priority
//!   articles

use std::net::SocketAddr;

use anyhow::Result;
use tokio::io::{AsyncReadExt, AsyncWriteExt};
use tokio::net::{TcpListener, TcpStream};
use tracing::{debug, error, info};

/// Default port for the vext groove endpoint.
pub const GROOVE_PORT: u16 = 6480;

/// Maximum HTTP request size (16 KiB).
const MAX_REQUEST_SIZE: usize = 16 * 1024;

/// Build the groove manifest JSON for vext.
///
/// Returns a JSON document conforming to the Groove v1 discovery schema.
/// The manifest advertises the `nntp-bridge` capability and declares
/// consumption of `octad-storage` and `voice-comms`.
fn manifest(port: u16) -> String {
    format!(
        r#"{{
  "groove_version": "1",
  "service_id": "vext",
  "service_version": "{}",
  "capabilities": {{
    "nntp_bridge": {{
      "type": "nntp-bridge",
      "description": "NNTP/Usenet bridge with hash chain attestation and multi-backend support",
      "protocol": "http",
      "endpoint": "/api/v1/nntp",
      "requires_auth": false,
      "panel_compatible": true
    }}
  }},
  "consumes": ["octad-storage", "voice-comms"],
  "endpoints": {{
    "api": "http://localhost:{}/api/v1",
    "health": "http://localhost:{}/health"
  }},
  "health": "/health",
  "applicability": ["individual", "team"]
}}"#,
        env!("CARGO_PKG_VERSION"),
        port,
        port
    )
}

/// Run the groove discovery HTTP server on the given port.
///
/// Uses tokio's async TCP listener to match vext's existing async runtime.
/// Spawns a task per connection so the groove endpoint never blocks the
/// main notification listener.
pub async fn run(port: u16) -> Result<()> {
    let addr: SocketAddr = format!("127.0.0.1:{}", port).parse()?;
    let listener = TcpListener::bind(addr).await?;
    info!("[groove] vext groove endpoint listening on {}", addr);
    info!(
        "[groove] Probe: curl http://localhost:{}/.well-known/groove",
        port
    );

    loop {
        match listener.accept().await {
            Ok((stream, peer)) => {
                debug!("[groove] Connection from {}", peer);
                tokio::spawn(async move {
                    if let Err(e) = handle_request(stream, port).await {
                        error!("[groove] Request error from {}: {}", peer, e);
                    }
                });
            }
            Err(e) => {
                error!("[groove] Accept error: {}", e);
            }
        }
    }
}

/// Handle a single groove HTTP request.
///
/// Parses the first line of the HTTP request to extract method and path,
/// then routes to the appropriate handler.
async fn handle_request(mut stream: TcpStream, port: u16) -> Result<()> {
    let mut buf = vec![0u8; MAX_REQUEST_SIZE];
    let n = stream.read(&mut buf).await?;
    if n == 0 {
        return Ok(());
    }

    let request = std::str::from_utf8(&buf[..n])?;
    let first_line = request.lines().next().unwrap_or("");
    let parts: Vec<&str> = first_line.split_whitespace().collect();

    if parts.len() < 2 {
        send_response(&mut stream, 400, "text/plain", "Bad Request").await?;
        return Ok(());
    }

    let method = parts[0];
    let path = parts[1];

    match (method, path) {
        // GET /.well-known/groove — Return the capability manifest.
        ("GET", "/.well-known/groove") => {
            let json = manifest(port);
            send_response(&mut stream, 200, "application/json", &json).await?;
        }

        // GET /health — Simple health check.
        ("GET", "/health") => {
            send_response(
                &mut stream,
                200,
                "application/json",
                r#"{"status":"ok","service":"vext"}"#,
            )
            .await?;
        }

        // Unknown route.
        _ => {
            send_response(&mut stream, 404, "text/plain", "Not Found").await?;
        }
    }

    Ok(())
}

/// Send an HTTP response with the given status, content type, and body.
async fn send_response(
    stream: &mut TcpStream,
    status: u16,
    content_type: &str,
    body: &str,
) -> Result<()> {
    let status_text = match status {
        200 => "OK",
        400 => "Bad Request",
        404 => "Not Found",
        _ => "Unknown",
    };
    let response = format!(
        "HTTP/1.0 {} {}\r\nContent-Type: {}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{}",
        status,
        status_text,
        content_type,
        body.len(),
        body
    );
    stream.write_all(response.as_bytes()).await?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_manifest_contains_service_id() {
        let json = manifest(6480);
        assert!(json.contains(r#""service_id": "vext""#));
    }

    #[test]
    fn test_manifest_contains_nntp_bridge_capability() {
        let json = manifest(6480);
        assert!(json.contains(r#""type": "nntp-bridge""#));
    }

    #[test]
    fn test_manifest_contains_consumes() {
        let json = manifest(6480);
        assert!(json.contains(r#""octad-storage""#));
        assert!(json.contains(r#""voice-comms""#));
    }

    #[test]
    fn test_manifest_contains_health_endpoint() {
        let json = manifest(6480);
        assert!(json.contains(r#""health": "/health""#));
    }

    #[test]
    fn test_manifest_port_substitution() {
        let json = manifest(9999);
        assert!(json.contains("http://localhost:9999/api/v1"));
        assert!(json.contains("http://localhost:9999/health"));
    }

    #[test]
    fn test_manifest_groove_version() {
        let json = manifest(6480);
        assert!(json.contains(r#""groove_version": "1""#));
    }
}
