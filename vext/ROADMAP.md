# Vext Implementation Roadmap

**Last Updated:** 2025-01-31

---

## Phase 1: Foundation & Research (Q1 2025) - CURRENT

### Goals
- [ ] Complete a2ml specification in Idris2
- [ ] Design cryptographic architecture
- [ ] Write arXiv paper
- [ ] NUJ Ethics Council presentation

### Milestones

#### a2ml Specification (Idris2)
- [ ] Define core types (Message, Commitment, Proof)
- [ ] Implement Merkle tree with dependent type proofs
- [ ] Prove chronological ordering at type level
- [ ] Prove non-injection (no ads can be inserted)
- [ ] Totality checking (all functions terminate)
- [ ] Generate specification document from Idris2 types

#### Cryptographic Design
- [ ] Threat model documentation
- [ ] Security proofs (on paper)
- [ ] Performance analysis
- [ ] Comparison with existing systems

#### Academic Publication
- [ ] arXiv paper draft (8 weeks)
- [ ] Implementation for benchmarks
- [ ] Submit to arXiv (cs.CR)
- [ ] Submit to ACM CCS 2026 (deadline: May 2025)

#### NUJ Engagement
- [ ] Present discussion paper to Ethics Council
- [ ] Gather feedback from journalists
- [ ] Refine use cases based on feedback

**Status:** Design phase, specification in progress

---

## Phase 2: Core Protocol (Q2 2025)

### Goals
- [ ] a2ml verification library (Idris2)
- [ ] NNTP integration
- [ ] Reference client (CLI)
- [ ] Formal verification complete

### Milestones

#### a2ml-idris Library
- [ ] Core verification functions
- [ ] Commitment generation
- [ ] Merkle proof generation
- [ ] Signature verification
- [ ] Export to Rust/C via FFI

#### NNTP Integration
- [ ] Choose NNTP server (INN vs custom Rust)
- [ ] Modify server to publish a2ml commitments
- [ ] Commitment publishing (HTTPS + Gemini + NNTP control)
- [ ] Testing with real newsgroups

#### Reference Client
- [ ] Fetch commitments from server
- [ ] Verify Merkle proofs
- [ ] Display verification status
- [ ] CLI interface (vext-cli)

#### Formal Verification
- [ ] All core functions proven in Idris2
- [ ] Property-based testing (QuickCheck)
- [ ] Fuzzing (for FFI boundary)
- [ ] Security audit

**Estimated Duration:** 12 weeks

---

## Phase 3: Gateway Implementation (Q3 2025)

### Goals
- [ ] Email gateway (production-ready)
- [ ] SMS gateway
- [ ] RCS gateway
- [ ] Matrix bridge (HIGH PRIORITY)

### Milestones

#### Email Gateway (Rust)
- [ ] SMTP server (incoming emails)
- [ ] IMAP polling (fallback)
- [ ] Mailing list functionality
- [ ] Subscription management
- [ ] Rate limiting
- [ ] Spam filtering

#### SMS Gateway (Rust)
- [ ] Twilio integration
- [ ] Message parsing (commands)
- [ ] Subscription via SMS
- [ ] Post via SMS
- [ ] Receive updates via SMS

#### RCS Gateway (Rust)
- [ ] Google RCS Business Messaging API
- [ ] Rich cards (formatted messages)
- [ ] Interactive buttons
- [ ] Media support
- [ ] Fallback to SMS

#### Matrix Bridge (HIGH PRIORITY)
- [ ] Matrix protocol client
- [ ] Bidirectional message sync
- [ ] Room per vext tag
- [ ] E2E encryption support
- [ ] Integration with Matrix federation

**Estimated Duration:** 16 weeks

---

## Phase 4: Mobile & Web (Q4 2025)

### Goals
- [ ] PWA (Progressive Web App)
- [ ] Native mobile apps (Tauri/Dioxus)
- [ ] Web interface
- [ ] Public beta launch

### Milestones

#### PWA
- [ ] ReScript frontend
- [ ] Offline support
- [ ] Push notifications
- [ ] Install prompt
- [ ] Works on all platforms

#### Native Apps (Tauri 2.0)
- [ ] iOS app
- [ ] Android app
- [ ] Rust backend (shared with CLI)
- [ ] ReScript UI
- [ ] App store submission

#### Web Interface
- [ ] Read feeds (public)
- [ ] Post messages (authenticated)
- [ ] Subscription management
- [ ] Verification status display
- [ ] Gemini/Gopher gateway

#### Public Beta
- [ ] Invite-only testing (100 users)
- [ ] Feedback collection
- [ ] Bug fixes
- [ ] Performance optimization
- [ ] Security audit

**Estimated Duration:** 16 weeks

---

## Phase 5: Federation & Scale (Q1 2026)

### Goals
- [ ] Multi-server federation
- [ ] Distributed commitments
- [ ] Server diversity
- [ ] 10,000+ users

### Milestones

#### Server Federation
- [ ] NNTP flooding (server-to-server)
- [ ] Commitment synchronization
- [ ] Cross-server verification
- [ ] Byzantine fault tolerance

#### Distributed Commitments
- [ ] Multiple servers co-sign commitments
- [ ] Threshold signatures (k-of-n)
- [ ] Consensus mechanism
- [ ] Censorship resistance

#### Server Software
- [ ] Easy deployment (Docker)
- [ ] Admin interface
- [ ] Monitoring/metrics
- [ ] Documentation for server operators

#### Scale Testing
- [ ] Load testing (10K concurrent users)
- [ ] Performance optimization
- [ ] CDN integration (if needed)
- [ ] Cost analysis

**Estimated Duration:** 12 weeks

---

## Phase 6: Ecosystem Growth (Q2-Q4 2026)

### Goals
- [ ] Protocol standardization (RFC?)
- [ ] Third-party clients
- [ ] Integration with other platforms
- [ ] 100,000+ users

### Milestones

#### Standardization
- [ ] IETF RFC submission (a2ml specification)
- [ ] W3C consideration (if applicable)
- [ ] Academic citations
- [ ] Industry adoption

#### Client Diversity
- [ ] Encourage third-party clients
- [ ] Client certification program
- [ ] Reference implementations in multiple languages
- [ ] Accessibility-focused clients

#### Platform Integrations
- [ ] Nostr bridge
- [ ] XMPP bridge
- [ ] ActivityPub bridge (Mastodon interop)
- [ ] WebMention support

#### Sustainability
- [ ] Funding model (grants, donations, sponsorships)
- [ ] Governance structure
- [ ] Community building
- [ ] Long-term maintenance plan

**Estimated Duration:** Ongoing

---

## Critical Path

**Must happen in order:**

1. ✅ **a2ml specification** (Idris2) → Everything depends on this
2. **NNTP integration** → Proves protocol works
3. **Matrix bridge** → Universal access (highest leverage)
4. **Mobile apps** → User adoption
5. **Federation** → Censorship resistance

**Parallel tracks:**
- Email/SMS/RCS gateways (can happen alongside NNTP)
- Web interface (can happen alongside mobile)
- Research publication (ongoing)

---

## Success Metrics

### Phase 1 (Research)
- [ ] arXiv paper published
- [ ] NUJ endorsement
- [ ] 10+ academic citations

### Phase 2 (Core Protocol)
- [ ] a2ml library formally verified
- [ ] NNTP server running in production
- [ ] 100+ messages verified

### Phase 3 (Gateways)
- [ ] Matrix bridge operational
- [ ] Email gateway handling 1000+ messages/day
- [ ] SMS/RCS gateways tested

### Phase 4 (Mobile & Web)
- [ ] 1,000+ beta users
- [ ] Apps in app stores
- [ ] <1% crash rate

### Phase 5 (Federation)
- [ ] 10+ federated servers
- [ ] 10,000+ active users
- [ ] 99.9% uptime

### Phase 6 (Ecosystem)
- [ ] RFC published
- [ ] 5+ third-party clients
- [ ] 100,000+ users

---

## Risk Mitigation

### Technical Risks

**Risk:** Idris2 learning curve too steep
**Mitigation:** Start with small proofs, engage Idris2 community, hire expert

**Risk:** NNTP is "too old" / not adopted
**Mitigation:** Matrix bridge provides modern interface, NNTP is just transport

**Risk:** Performance overhead of verification
**Mitigation:** Benchmarking shows <0.5% overhead, acceptable trade-off

### Adoption Risks

**Risk:** "Too technical" for average users
**Mitigation:** Simple clients hide complexity, focus on UX

**Risk:** Network effects favor existing platforms
**Mitigation:** Start with niche (journalists), expand gradually

**Risk:** Lack of funding
**Mitigation:** Apply for grants (NLnet, Prototype Fund), crowdfunding

### Security Risks

**Risk:** Cryptographic vulnerability discovered
**Mitigation:** Use proven algorithms (Ed25519, SHA-256), formal verification

**Risk:** Implementation bugs
**Mitigation:** Formal verification (Idris2), fuzzing, security audits

**Risk:** Social engineering attacks
**Mitigation:** User education, clear security indicators in UI

---

## Resource Requirements

### Personnel

**Phase 1-2 (Q1-Q2 2025):**
- 1 Idris2 expert (a2ml specification)
- 1 Rust developer (implementation)
- 1 cryptography expert (review)

**Phase 3-4 (Q3-Q4 2025):**
- 2 Rust developers (gateways + mobile)
- 1 ReScript developer (UI)
- 1 designer (UX/UI)

**Phase 5-6 (2026):**
- 3 backend developers (scaling)
- 2 frontend developers (clients)
- 1 community manager

### Funding

**Estimated costs:**
- Phase 1-2: €50K (salaries, infrastructure)
- Phase 3-4: €100K
- Phase 5-6: €200K

**Potential sources:**
- NLnet Foundation (Next Generation Internet)
- Prototype Fund (BMBF, Germany)
- NUJ (if endorsed)
- Crowdfunding (Kickstarter, OpenCollective)
- Grants (Mozilla, Sovereign Tech Fund)

---

## Next Actions (This Month)

1. **Complete a2ml specification** (Idris2 types)
2. **Write arXiv paper draft** (sections 1-4)
3. **Present to NUJ Ethics Council**
4. **Implement Merkle tree proof-of-concept** (Idris2)
5. **Set up development infrastructure** (CI/CD, repo structure)

---

## Long-Term Vision (5+ years)

**Vext becomes:**
- Default protocol for journalist communication
- Required for academic publishing (provable attribution)
- Standard for verified news distribution
- Platform-independent social infrastructure
- Regulatory compliance standard (EU DSA)

**Success looks like:**
- Millions of users
- Hundreds of federated servers
- Dozens of client implementations
- Cited in legislation
- Taught in journalism schools

---

**"The future is chronological, verifiable, and ad-free."**
