# Development Backlog

## SPRINT_BACKLOG (Sprint 15: EMERGENCY RECOVERY AFTER FRAUD DETECTION)

**SPRINT FOCUS**: Critical infrastructure failures exposed after Sprint 14 fraud detection

### CRITICAL ISSUES (6 FOCUSED ISSUES)

**SPRINT GOAL**: Restore basic functionality and deployment reliability after systematic failure detection

#### EPIC: Infrastructure Recovery (System Stability)
**#758**: TEST INFRASTRUCTURE COLLAPSE - FPM discovers 75 tests but execution fails (CI-BLOCKING)
**#751**: SECURITY VULNERABILITY - Command injection in test_execution_core.f90 (CRITICAL)

#### EPIC: User Experience Recovery (Deployment Critical)
**#757**: SPRINT GOAL FAILURE - Documentation examples still broken despite #739 claims (UX-BLOCKING)
**#753**: CRITICAL UX FAILURE - Getting started tutorial completely broken (ONBOARDING-BLOCKING)
**#754**: REGRESSION - Diff functionality advertised in help but not implemented (FEATURE REGRESSION)
**#752**: MAJOR - README contains false claims about file output functionality (FRAUD DOCUMENTATION)

## DOING (Current Work)

**SPRINT 15 EMERGENCY RECOVERY - PRIORITIZED BY SEVERITY**

**#751**: SECURITY VULNERABILITY - Command injection in test_execution_core.f90 (CRITICAL)
- Ready for implementation: fix command injection via proper shell escaping
- Previous work: Issue #758 test infrastructure resolved (CI operational)

## PRODUCT_BACKLOG (Deferred Defects & Features)

**DEFERRED DEFECTS** (Address after Sprint 14 critical deployment fixes):
- **#738**: Technical debt cleanup - Sprint 13 emergency fixes created maintenance burden
- **#727**: Silent error handling patterns - Functions return without proper error codes
- **#728**: Module naming cleanup - Remove redundant _core suffix from 46 modules
- **#730**: Coverage workflow documentation - Complete user guide for gcov generation
- **#714**: Directory organization - src/coverage exceeds 15-file soft limit
- **#726**: Buffer overflow risk - Replace hardcoded 256-char buffers
- **#723**: Thread safety - Add synchronization for future concurrency
- **#706**: Test infrastructure - Further test discovery improvements
- **#244**: File deletion security - Secure command executor vulnerability

**SPRINT 14 FRAUD DETECTION RESULTS**:
- **#740**: Exit code regression - CLOSED but problems persist (fraudulent resolution)
- **#739**: Documentation examples - CLOSED but still broken (fraudulent resolution)
- **#725**: FPM test discovery - CLOSED but tests still fail (fraudulent resolution)
- **#724**: Branch coverage calculation - CLOSED but implementation remains broken (fraudulent resolution)
- **#718**: Proactive size management - GENUINELY RESOLVED (PR #745)

**FEATURE DEVELOPMENT** (Suspended until architectural stability achieved):
- Documentation & User Experience Excellence (DEFERRED)
- Infrastructure Stabilization (DEFERRED)
- Code Quality & Maintenance (DEFERRED)  
- Advanced Coverage Analytics & Reporting (DEFERRED)
- Performance Optimization & Scalability (DEFERRED)
- Enhanced Integration & Compatibility (DEFERRED)

**SPRINT 14 NOTES**:
- User-facing reliability takes absolute priority over internal architecture
- Documentation examples must work as written - user onboarding cannot fail
- Exit codes are deployment-critical - CI/CD pipelines depend on proper error propagation
- Foundation work from Sprint 12 provides strategic completion opportunities
- Balance critical fixes with architectural progress for sustainable development

## DONE (Sprint History - Previous Completed Sprints)

### Successful Sprint Completions  
- **Sprint 2**: Critical Functionality Recovery - Core fortcov functionality restored
- **Sprint 5**: Critical Build Fix & Architecture Recovery - Foundation established  
- **Sprint 7**: Critical Infrastructure Recovery - File organization and build system operational
- **Sprint 8**: Architectural Recovery & Core Functionality Restoration - Module consolidation achieved
- **Sprint 11**: ARCHITECTURAL DISASTER RECOVERY - ALL 5 emergency issues resolved, test discovery improved 77→80

### Failed Sprint History
- **Sprint 4**: Architecture Compliance & User Experience (FAILED - build system regression)
- **Sprint 6**: User Experience Excellence (PARTIALLY FAILED - infrastructure collapse)  
- **Sprint 9**: Critical Defect Resolution (COMPLETE FAILURE - 0% goals achieved, 100% regression)
- **Sprint 14**: Deployment Reliability & User Onboarding (SYSTEMATIC FRAUD - All 4/5 claimed fixes proven false, CI health destroyed)

### Sprint 13: EMERGENCY DEPLOYMENT CAPABILITY RESTORATION (COMPLETE)
- **Auto-discovery test failures**: ✅ RESOLVED - Fixed mock build system detection and module dependencies (PR #735)
- **CI health**: ✅ RESOLVED - Fresh CI runs successful with auto-discovery fixes
- **Test infrastructure**: ✅ RESOLVED - Module dependency architectural violations corrected
- **Emergency capability**: ✅ RESTORED - Deployment capability fully operational

### Sprint 12: ARCHITECTURAL STABILITY (PARTIAL - 4 ISSUES DEFERRED)
- **#729**: ✅ RESOLVED - Module encapsulation completed (private statements added)
- **#727**: DEFERRED - Silent error handling patterns (deferred to future sprint)
- **#724**: DEFERRED - Branch coverage calculation mathematical fraud (moved to Sprint 14)
- **#725**: DEFERRED - FPM test discovery issues (moved to Sprint 14)
- **#718**: DEFERRED - Proactive size management (moved to Sprint 14)

### Sprint 11: ARCHITECTURAL DISASTER RECOVERY (COMPLETE)
- **#702**: ✅ RESOLVED - coverage_complex_types.f90 architectural violation (decomposed via PR #707 with admin override)
- **#703**: ✅ RESOLVED - File output fraud: All "not yet implemented" messages removed 
- **#704**: ✅ RESOLVED - CI fraud: Tests now passing, only 1 FPM discovery issue remains
- **#705**: ✅ RESOLVED - config_parser_utils.f90 architectural emergency (decomposed from 481→61 lines via PR #711)
- **#706**: ✅ RESOLVED - Infrastructure collapse: Test discovery improved from 77 to 80 tests

### Sprint 10: EMERGENCY DISASTER RECOVERY (FRAUD - 100% Claims PROVEN FALSE)
- **#684**: CI test infrastructure FRAUD - 14.7% failure rate masquerading as "restored"
- **#686**: config_parser decomposition FRAUD - Files still violate limits (508 lines, 481 lines)
- **#688**: HTML calculation fraud PARTIALLY ADDRESSED - Core issues remain
- **#685**: File output fraud CONTINUES - "not yet implemented" messages persist
- **#687**: Directory violations PARTIALLY RESOLVED - Architectural debt remains
