# Threshold-Based Coverage Diff Algorithm Design

## Problem Statement

Patrick's audit identified that fortcov uses oversimplified binary comparison for coverage diff analysis, lacking the sophisticated threshold-based analysis found in industry-standard tools like pycobertura and gcovr. This design document outlines improvements to align with industry standards.

## Current Issues

1. **Binary Comparison**: Simple presence/absence checks without significance analysis
2. **Missing Classification**: No sophisticated change categorization
3. **No Multi-Level Thresholds**: Single threshold instead of tiered analysis
4. **Insufficient Context**: Limited consideration of statistical significance

## Proposed Threshold-Based Algorithm

### 1. Multi-Tiered Classification System

Instead of simple binary comparison, implement significance-based classification:

```
CRITICAL_DEGRADATION:  Coverage decrease ≥ 5.0%
MAJOR_DEGRADATION:     Coverage decrease ≥ 2.0% and < 5.0%
MINOR_DEGRADATION:     Coverage decrease ≥ 0.5% and < 2.0%
UNCHANGED:             Coverage change < 0.5%
MINOR_IMPROVEMENT:     Coverage increase ≥ 0.5% and < 2.0%
MAJOR_IMPROVEMENT:     Coverage increase ≥ 2.0% and < 5.0%
CRITICAL_IMPROVEMENT:  Coverage increase ≥ 5.0%
NEW_FILE:              File added with coverage > 0%
REMOVED_FILE:          File removed
NEW_FUNCTION:          Function added with coverage > 0%
REMOVED_FUNCTION:      Function removed
```

### 2. Configurable Threshold System

```fortran
type :: diff_thresholds_t
    real :: critical_threshold = 5.0      ! ≥5% change
    real :: major_threshold = 2.0         ! ≥2% change  
    real :: minor_threshold = 0.5         ! ≥0.5% change
    real :: significance_threshold = 0.1  ! Minimum for reporting
end type
```

### 3. Statistical Context Analysis

For each change, analyze:
- **Absolute Change**: Percentage point difference
- **Relative Change**: Proportional change considering baseline
- **Statistical Significance**: Based on line count and variance
- **Trend Classification**: Consistent improvement/degradation patterns

### 4. Multi-Level Analysis Hierarchy

```
Project Level:   Overall coverage trend across all files
Package Level:   Directory/module-based aggregation  
File Level:      Per-file coverage analysis
Function Level:  Individual function coverage changes
Line Level:      Specific line-by-line changes
```

### 5. Context-Aware Reporting

Classifications consider:
- **Baseline Coverage**: Higher changes more significant at high baselines
- **File Size**: Weight significance by executable line count
- **Change Magnitude**: Absolute vs relative importance
- **Historical Context**: Trend analysis where available

## Algorithm Implementation

### Core Classification Function

```fortran
function classify_coverage_change(baseline_pct, current_pct, thresholds) result(classification)
    real, intent(in) :: baseline_pct, current_pct
    type(diff_thresholds_t), intent(in) :: thresholds
    integer :: classification
    
    real :: abs_change, rel_change
    
    abs_change = current_pct - baseline_pct
    
    ! Handle edge cases
    if (baseline_pct == 0.0 .and. current_pct > 0.0) then
        classification = NEW_COVERAGE
        return
    else if (baseline_pct > 0.0 .and. current_pct == 0.0) then
        classification = LOST_COVERAGE  
        return
    end if
    
    ! Apply threshold-based classification
    if (abs(abs_change) < thresholds%significance_threshold) then
        classification = UNCHANGED
    else if (abs_change >= thresholds%critical_threshold) then
        classification = merge(CRITICAL_IMPROVEMENT, CRITICAL_DEGRADATION, abs_change > 0)
    else if (abs_change >= thresholds%major_threshold) then
        classification = merge(MAJOR_IMPROVEMENT, MAJOR_DEGRADATION, abs_change > 0)
    else if (abs_change >= thresholds%minor_threshold) then
        classification = merge(MINOR_IMPROVEMENT, MINOR_DEGRADATION, abs_change > 0)
    else
        classification = UNCHANGED
    end if
end function
```

### Enhanced File Diff Analysis

```fortran
function compute_enhanced_file_diff(baseline_file, current_file, thresholds) result(file_diff)
    ! 1. Calculate basic coverage percentages
    ! 2. Apply threshold-based classification
    ! 3. Compute statistical significance metrics
    ! 4. Analyze line-level changes for context
    ! 5. Generate summary with confidence indicators
end function
```

### Aggregation Strategy

1. **Bottom-Up Analysis**: Start with line-level changes
2. **Hierarchical Rollup**: Aggregate to function, file, and project levels
3. **Weighted Significance**: Consider size and coverage context
4. **Summary Classification**: Project-level trend determination

## Benefits of Threshold-Based Approach

1. **Industry Alignment**: Matches sophisticated tools like pycobertura
2. **Actionable Insights**: Clear categorization of change significance
3. **Noise Reduction**: Filters insignificant fluctuations
4. **Context Awareness**: Considers statistical significance
5. **Configurable Sensitivity**: Adjustable thresholds for different needs

## Implementation Phases

### Phase 1: Enhanced Classification Engine
- Implement multi-tiered classification system
- Add configurable threshold types
- Create classification functions

### Phase 2: Statistical Context Analysis  
- Add significance testing
- Implement weighted analysis
- Consider baseline context

### Phase 3: Multi-Level Aggregation
- Hierarchical change analysis
- Project/file/function level summaries
- Trend analysis capabilities

### Phase 4: Advanced Reporting
- Context-aware change descriptions
- Confidence indicators
- Actionable recommendations

## Testing Strategy

1. **Unit Tests**: Classification logic with edge cases
2. **Integration Tests**: End-to-end diff scenarios
3. **Comparison Tests**: Validation against reference tools
4. **Performance Tests**: Scalability with large codebases

## Compatibility

- Maintains existing API for backward compatibility
- Adds new threshold-based methods as extensions
- Configurable to match different tool behaviors
- Graceful degradation for missing data

This threshold-based algorithm provides sophisticated coverage analysis that matches industry standards while remaining configurable and performant.