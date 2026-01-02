#!/usr/bin/env python3
"""
6d_DescribeInfoCovMatched.py

Analyze and visualize information covenant usage for the propensity score matched samples.

Creates time series plots showing:
1. Information covenant usage over time by direct vs non-direct loans (matched sample)
2. Detailed breakdown of covenant types for direct vs non-direct loans (matched samples)
3. Comparison of covenant usage between matched direct and non-direct loans
4. Comparison of nonbank direct vs bank direct loans within the direct matched sample

Inputs:
- ../Data/Intermediate/5d_MatchedPanelLoans_Direct.csv (direct vs non-direct matched sample)
- ../Data/Intermediate/6b_PanelWithInfoCovenants.csv (full panel with covenants)

Outputs:
- ../Results/Figures/6d_InfoCovenants_Matched_Direct_vs_NonDirect.png
- ../Results/Figures/6d_InfoCovenants_Matched_Direct_Detailed.png
- ../Results/Figures/6d_InfoCovenants_Matched_Direct_Comparison.png
- ../Results/Figures/6d_InfoCovenants_Matched_NonbankDirect_vs_BankDirect.png

Author: Zirui Song
Date: Oct 2025
"""

import pandas as pd
import numpy as np
from pathlib import Path
import plotly.graph_objects as go
from plotly.subplots import make_subplots
import plotly.io as pio
import matplotlib.pyplot as plt
import seaborn as sns
from scipy import stats


def ensure_fig_dir() -> Path:
    """Ensure the figures directory exists."""
    fig_dir = Path("/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending/Results/Figures")
    fig_dir.mkdir(parents=True, exist_ok=True)
    return fig_dir


def extract_year_from_date(date_col, df):
    """Extract year from various date formats."""
    if date_col not in df.columns:
        return None
    
    # Try different date parsing methods
    date_series = df[date_col].copy()
    
    # Convert to string and extract year
    date_series = date_series.astype(str)
    
    # Extract year from various formats
    def extract_year(date_str):
        if pd.isna(date_str) or date_str == 'nan' or date_str == '':
            return None
        
        # Try to extract 4-digit year
        import re
        year_match = re.search(r'(19|20)\d{2}', str(date_str))
        if year_match:
            return int(year_match.group())
        return None
    
    return date_series.apply(extract_year)


def load_direct_matched_sample_with_covenants(matched_file, panel_file):
    """Load direct matched sample and merge with covenant data."""
    print("Loading direct matched sample...")
    df_matched = pd.read_csv(matched_file)
    print(f"  Direct matched sample: {len(df_matched)} pairs")
    
    print("Loading panel with covenants...")
    df_panel = pd.read_csv(panel_file)
    print(f"  Panel with covenants: {len(df_panel)} observations")
    
    # Get loan IDs from matched sample
    direct_loan_ids = df_matched['loan_id_direct'].tolist()
    nondirect_loan_ids = df_matched['loan_id_nondirect'].tolist()
    all_loan_ids = direct_loan_ids + nondirect_loan_ids
    
    # Filter panel to matched loans only
    df_matched_loans = df_panel[df_panel['loan_id'].isin(all_loan_ids)].copy()
    print(f"  Matched loans with covenant data: {len(df_matched_loans)}")
    
    # Create a mapping from loan_id to pair information
    pair_info = {}
    for _, row in df_matched.iterrows():
        pair_info[row['loan_id_direct']] = {
            'ff12': row['ff12'],
            'term_loan': row['term_loan'],
            'is_direct_pair': True
        }
        pair_info[row['loan_id_nondirect']] = {
            'ff12': row['ff12'],
            'term_loan': row['term_loan'],
            'is_direct_pair': False
        }
    
    # Add pair information to matched loans
    df_matched_loans['ff12'] = df_matched_loans['loan_id'].map(lambda x: pair_info.get(x, {}).get('ff12'))
    df_matched_loans['term_loan'] = df_matched_loans['loan_id'].map(lambda x: pair_info.get(x, {}).get('term_loan'))
    df_matched_loans['is_direct_pair'] = df_matched_loans['loan_id'].map(lambda x: pair_info.get(x, {}).get('is_direct_pair', False))
    
    # Create direct indicator from direct_from_text if available
    if 'direct_from_text' in df_matched_loans.columns:
        df_matched_loans['direct'] = df_matched_loans['direct_from_text'].fillna(0).astype(int)
    else:
        # Infer from is_direct_pair
        df_matched_loans['direct'] = df_matched_loans['is_direct_pair'].astype(int)
    
    return df_matched_loans


def create_direct_matched_time_series(df, fig_dir):
    """Create time series plots comparing direct vs non-direct covenant usage in matched sample."""
    
    # Calculate annual covenant usage rates
    direct_data = df[df['direct'] == 1].copy()
    nondirect_data = df[df['direct'] == 0].copy()
    
    # Group by year and calculate rates
    direct_yearly = direct_data.groupby('year').agg({
        'monthly_fs': 'mean',
        'projected_fs': 'mean', 
        'lender_meeting': 'mean',
        'total_info_covenants': ['mean', 'count']
    }).reset_index()
    
    nondirect_yearly = nondirect_data.groupby('year').agg({
        'monthly_fs': 'mean',
        'projected_fs': 'mean',
        'lender_meeting': 'mean', 
        'total_info_covenants': ['mean', 'count']
    }).reset_index()
    
    # Flatten column names
    direct_yearly.columns = ['year', 'monthly_fs_rate', 'projected_fs_rate', 'meeting_rate', 'avg_covenants', 'count']
    nondirect_yearly.columns = ['year', 'monthly_fs_rate', 'projected_fs_rate', 'meeting_rate', 'avg_covenants', 'count']
    
    # Filter years with sufficient data (at least 5 observations for matched sample)
    direct_yearly = direct_yearly[direct_yearly['count'] >= 5]
    nondirect_yearly = nondirect_yearly[nondirect_yearly['count'] >= 5]
    
    # Create subplot
    fig = make_subplots(
        rows=2, cols=2,
        subplot_titles=('Monthly FS Requirements', 'Projected FS Requirements', 
                       'Lender Meeting Requirements', 'Average Total Covenants'),
        vertical_spacing=0.12
    )
    
    # Plot 1: Monthly FS
    fig.add_trace(
        go.Scatter(x=direct_yearly['year'], y=direct_yearly['monthly_fs_rate'], 
                  name='Direct (Matched)', line=dict(color='green'), mode='lines+markers'),
        row=1, col=1
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['monthly_fs_rate'],
                  name='Non-Direct (Matched)', line=dict(color='orange'), mode='lines+markers'),
        row=1, col=1
    )
    
    # Plot 2: Projected FS
    fig.add_trace(
        go.Scatter(x=direct_yearly['year'], y=direct_yearly['projected_fs_rate'],
                  name='Direct (Matched)', line=dict(color='green'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['projected_fs_rate'],
                  name='Non-Direct (Matched)', line=dict(color='orange'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    
    # Plot 3: Lender Meetings
    fig.add_trace(
        go.Scatter(x=direct_yearly['year'], y=direct_yearly['meeting_rate'],
                  name='Direct (Matched)', line=dict(color='green'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['meeting_rate'],
                  name='Non-Direct (Matched)', line=dict(color='orange'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    
    # Plot 4: Average Total Covenants
    fig.add_trace(
        go.Scatter(x=direct_yearly['year'], y=direct_yearly['avg_covenants'],
                  name='Direct (Matched)', line=dict(color='green'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['avg_covenants'],
                  name='Non-Direct (Matched)', line=dict(color='orange'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    
    fig.update_layout(
        title='Information Covenant Usage Over Time: Matched Direct vs Non-Direct Loans',
        height=800,
        title_x=0.5
    )
    
    # Update y-axis labels
    fig.update_yaxes(title_text="Usage Rate", row=1, col=1)
    fig.update_yaxes(title_text="Usage Rate", row=1, col=2)
    fig.update_yaxes(title_text="Usage Rate", row=2, col=1)
    fig.update_yaxes(title_text="Average Count", row=2, col=2)
    
    fig.update_xaxes(title_text="Year", row=2, col=1)
    fig.update_xaxes(title_text="Year", row=2, col=2)
    
    # Save plot
    pio.write_image(fig, str(fig_dir / "6d_InfoCovenants_Matched_Direct_vs_NonDirect.png"), 
                   width=1200, height=800, scale=2)


def create_direct_matched_detailed_comparison_plots(df, fig_dir):
    """Create detailed comparison plots for direct matched sample using matplotlib/seaborn."""
    
    # Set style
    plt.style.use('default')
    sns.set_palette("husl")
    
    # Create figure with subplots
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('Information Covenant Usage: Matched Direct vs Non-Direct Sample Analysis', fontsize=16, y=0.98)
    
    # 1. Covenant usage by direct vs non-direct
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS', 'Projected FS', 'Lender Meeting']
    
    direct_rates = []
    nondirect_rates = []
    
    for covenant in covenant_types:
        direct_rate = df[df['direct'] == 1][covenant].mean()
        nondirect_rate = df[df['direct'] == 0][covenant].mean()
        direct_rates.append(direct_rate)
        nondirect_rates.append(nondirect_rate)
    
    x = np.arange(len(covenant_names))
    width = 0.35
    
    axes[0, 0].bar(x - width/2, direct_rates, width, label='Direct (Matched)', alpha=0.8, color='green')
    axes[0, 0].bar(x + width/2, nondirect_rates, width, label='Non-Direct (Matched)', alpha=0.8, color='orange')
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage: Matched Direct vs Non-Direct')
    axes[0, 0].set_xticks(x)
    axes[0, 0].set_xticklabels(covenant_names, rotation=45)
    axes[0, 0].legend()
    axes[0, 0].grid(True, alpha=0.3)
    
    # 2. Covenant intensity distribution
    direct_intensity = df[df['direct'] == 1]['total_info_covenants'].value_counts().sort_index()
    nondirect_intensity = df[df['direct'] == 0]['total_info_covenants'].value_counts().sort_index()
    
    x_intensity = np.arange(4)
    direct_counts = [direct_intensity.get(i, 0) for i in range(4)]
    nondirect_counts = [nondirect_intensity.get(i, 0) for i in range(4)]
    
    # Normalize to percentages
    direct_total = sum(direct_counts)
    nondirect_total = sum(nondirect_counts)
    direct_pct = [count/direct_total*100 for count in direct_counts]
    nondirect_pct = [count/nondirect_total*100 for count in nondirect_counts]
    
    axes[0, 1].bar(x_intensity - width/2, direct_pct, width, label='Direct (Matched)', alpha=0.8, color='green')
    axes[0, 1].bar(x_intensity + width/2, nondirect_pct, width, label='Non-Direct (Matched)', alpha=0.8, color='orange')
    axes[0, 1].set_xlabel('Number of Covenant Types')
    axes[0, 1].set_ylabel('Percentage of Loans')
    axes[0, 1].set_title('Covenant Intensity Distribution (Matched)')
    axes[0, 1].set_xticks(x_intensity)
    axes[0, 1].set_xticklabels(['0', '1', '2', '3'])
    axes[0, 1].legend()
    axes[0, 1].grid(True, alpha=0.3)
    
    # 3. Statistical significance test results
    test_results = []
    for covenant in covenant_types:
        direct_values = df[df['direct'] == 1][covenant].values
        nondirect_values = df[df['direct'] == 0][covenant].values
        
        if len(direct_values) > 0 and len(nondirect_values) > 0:
            t_stat, p_value = stats.ttest_ind(direct_values, nondirect_values)
            test_results.append({
                'Covenant': covenant.replace('_', ' ').title(),
                'T-statistic': t_stat,
                'P-value': p_value,
                'Significant': p_value < 0.05
            })
    
    # Create a simple text summary
    axes[1, 0].text(0.1, 0.8, 'Statistical Tests (T-tests):', fontsize=12, fontweight='bold', transform=axes[1, 0].transAxes)
    
    y_pos = 0.7
    for result in test_results:
        significance = "***" if result['P-value'] < 0.01 else "**" if result['P-value'] < 0.05 else "*" if result['P-value'] < 0.1 else ""
        text = f"{result['Covenant']}: p={result['P-value']:.3f}{significance}"
        axes[1, 0].text(0.1, y_pos, text, fontsize=10, transform=axes[1, 0].transAxes)
        y_pos -= 0.1
    
    axes[1, 0].text(0.1, y_pos-0.1, "*** p<0.01, ** p<0.05, * p<0.1", fontsize=8, transform=axes[1, 0].transAxes)
    axes[1, 0].set_title('Statistical Significance Tests')
    axes[1, 0].axis('off')
    
    # 4. Time trend of overall covenant usage
    yearly_trend = df.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
    yearly_trend = yearly_trend[yearly_trend['count'] >= 5]  # Filter years with sufficient data
    
    ax_twin = axes[1, 1].twinx()
    line1 = axes[1, 1].plot(yearly_trend['year'], yearly_trend['mean'], 'g-o', label='Avg Covenants')
    line2 = ax_twin.plot(yearly_trend['year'], yearly_trend['count'], 'orange', marker='s', linestyle='-', label='Loan Count')
    
    axes[1, 1].set_xlabel('Year')
    axes[1, 1].set_ylabel('Average Covenants', color='g')
    ax_twin.set_ylabel('Number of Loans', color='orange')
    axes[1, 1].set_title('Covenant Usage Trend Over Time (Matched)')
    axes[1, 1].grid(True, alpha=0.3)
    
    # Combine legends
    lines = line1 + line2
    labels = [l.get_label() for l in lines]
    axes[1, 1].legend(lines, labels, loc='upper left')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6d_InfoCovenants_Matched_Direct_Detailed.png", dpi=300, bbox_inches='tight')
    plt.close()


def create_direct_matched_pairs_comparison(df, fig_dir):
    """Create comparison plots showing matched direct vs non-direct pairs side by side."""
    
    # Create figure
    fig, axes = plt.subplots(2, 2, figsize=(15, 10))
    fig.suptitle('Matched Pairs: Direct vs Non-Direct Covenant Comparison', fontsize=16, y=0.98)
    
    # Get matched pairs data
    direct_pairs = df[df['is_direct_pair'] == True].copy()
    nondirect_pairs = df[df['is_direct_pair'] == False].copy()
    
    # Ensure we have the same number of pairs
    min_pairs = min(len(direct_pairs), len(nondirect_pairs))
    direct_pairs = direct_pairs.head(min_pairs)
    nondirect_pairs = nondirect_pairs.head(min_pairs)
    
    # 1. Covenant usage comparison
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS', 'Projected FS', 'Lender Meeting']
    
    direct_rates = [direct_pairs[covenant].mean() for covenant in covenant_types]
    nondirect_rates = [nondirect_pairs[covenant].mean() for covenant in covenant_types]
    
    x = np.arange(len(covenant_names))
    width = 0.35
    
    axes[0, 0].bar(x - width/2, nondirect_rates, width, label='Non-Direct Pairs', alpha=0.8, color='orange')
    axes[0, 0].bar(x + width/2, direct_rates, width, label='Direct Pairs', alpha=0.8, color='green')
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage: Matched Pairs')
    axes[0, 0].set_xticks(x)
    axes[0, 0].set_xticklabels(covenant_names, rotation=45)
    axes[0, 0].legend()
    axes[0, 0].grid(True, alpha=0.3)
    
    # 2. Average total covenants
    avg_covenants = [nondirect_pairs['total_info_covenants'].mean(), direct_pairs['total_info_covenants'].mean()]
    axes[0, 1].bar(['Non-Direct Pairs', 'Direct Pairs'], avg_covenants, alpha=0.8, color=['orange', 'green'])
    axes[0, 1].set_ylabel('Average Total Covenants')
    axes[0, 1].set_title('Average Covenant Count: Matched Pairs')
    axes[0, 1].grid(True, alpha=0.3)
    
    # 3. Covenant intensity distribution comparison
    direct_intensity = direct_pairs['total_info_covenants'].value_counts().sort_index()
    nondirect_intensity = nondirect_pairs['total_info_covenants'].value_counts().sort_index()
    
    x_intensity = np.arange(4)
    direct_counts = [direct_intensity.get(i, 0) for i in range(4)]
    nondirect_counts = [nondirect_intensity.get(i, 0) for i in range(4)]
    
    # Normalize to percentages
    direct_total = sum(direct_counts)
    nondirect_total = sum(nondirect_counts)
    direct_pct = [count/direct_total*100 for count in direct_counts]
    nondirect_pct = [count/nondirect_total*100 for count in nondirect_counts]
    
    axes[1, 0].bar(x_intensity - width/2, nondirect_pct, width, label='Non-Direct Pairs', alpha=0.8, color='orange')
    axes[1, 0].bar(x_intensity + width/2, direct_pct, width, label='Direct Pairs', alpha=0.8, color='green')
    axes[1, 0].set_xlabel('Number of Covenant Types')
    axes[1, 0].set_ylabel('Percentage of Pairs')
    axes[1, 0].set_title('Covenant Intensity Distribution: Matched Pairs')
    axes[1, 0].set_xticks(x_intensity)
    axes[1, 0].set_xticklabels(['0', '1', '2', '3'])
    axes[1, 0].legend()
    axes[1, 0].grid(True, alpha=0.3)
    
    # 4. Statistical significance test results
    test_results = []
    for covenant in covenant_types:
        direct_values = direct_pairs[covenant].values
        nondirect_values = nondirect_pairs[covenant].values
        
        if len(direct_values) > 0 and len(nondirect_values) > 0:
            t_stat, p_value = stats.ttest_ind(direct_values, nondirect_values)
        test_results.append({
            'Covenant': covenant.replace('_', ' ').title(),
            'T-statistic': t_stat,
            'P-value': p_value,
            'Significant': p_value < 0.05
        })
    
    # Create a simple text summary
    axes[1, 1].text(0.1, 0.8, 'Statistical Tests (T-tests):', fontsize=12, fontweight='bold', transform=axes[1, 1].transAxes)
    
    y_pos = 0.7
    for result in test_results:
        significance = "***" if result['P-value'] < 0.01 else "**" if result['P-value'] < 0.05 else "*" if result['P-value'] < 0.1 else ""
        text = f"{result['Covenant']}: p={result['P-value']:.3f}{significance}"
        axes[1, 1].text(0.1, y_pos, text, fontsize=10, transform=axes[1, 1].transAxes)
        y_pos -= 0.1
    
    axes[1, 1].text(0.1, y_pos-0.1, "*** p<0.01, ** p<0.05, * p<0.1", fontsize=8, transform=axes[1, 1].transAxes)
    axes[1, 1].set_title('Statistical Significance Tests')
    axes[1, 1].axis('off')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6d_InfoCovenants_Matched_Direct_Comparison.png", dpi=300, bbox_inches='tight')
    plt.close()


def create_nonbank_direct_vs_bank_direct_comparison(df, fig_dir):
    """Create comparison plots for nonbank direct vs bank direct loans within the direct matched sample."""
    
    # Filter to direct loans only
    df_direct_only = df[df['direct'] == 1].copy()
    
    if len(df_direct_only) == 0:
        print("Warning: No direct loans found in matched sample")
        return
    
    # Check if nonbank_lender column exists
    if 'nonbank_lender' not in df_direct_only.columns:
        print("Warning: nonbank_lender column not found. Cannot compare nonbank direct vs bank direct.")
        return
    
    # Split into nonbank direct and bank direct
    nonbank_direct = df_direct_only[df_direct_only['nonbank_lender'] == 1].copy()
    bank_direct = df_direct_only[df_direct_only['nonbank_lender'] == 0].copy()
    
    print(f"\nNonbank Direct vs Bank Direct comparison:")
    print(f"  Nonbank Direct loans: {len(nonbank_direct)}")
    print(f"  Bank Direct loans: {len(bank_direct)}")
    
    if len(nonbank_direct) == 0 or len(bank_direct) == 0:
        print("Warning: Insufficient data for nonbank direct vs bank direct comparison")
        return
    
    # Set style
    plt.style.use('default')
    sns.set_palette("husl")
    
    # Create figure with subplots
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('Information Covenant Usage: Nonbank Direct vs Bank Direct Loans (Matched Direct Sample)', fontsize=16, y=0.98)
    
    # 1. Covenant usage comparison
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS', 'Projected FS', 'Lender Meeting']
    
    nonbank_direct_rates = []
    bank_direct_rates = []
    
    for covenant in covenant_types:
        nonbank_rate = nonbank_direct[covenant].mean()
        bank_rate = bank_direct[covenant].mean()
        nonbank_direct_rates.append(nonbank_rate)
        bank_direct_rates.append(bank_rate)
    
    x = np.arange(len(covenant_names))
    width = 0.35
    
    axes[0, 0].bar(x - width/2, bank_direct_rates, width, label='Bank Direct', alpha=0.8, color='teal')
    axes[0, 0].bar(x + width/2, nonbank_direct_rates, width, label='Nonbank Direct', alpha=0.8, color='purple')
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage: Nonbank Direct vs Bank Direct')
    axes[0, 0].set_xticks(x)
    axes[0, 0].set_xticklabels(covenant_names, rotation=45)
    axes[0, 0].legend()
    axes[0, 0].grid(True, alpha=0.3)
    
    # 2. Covenant intensity distribution
    nonbank_direct_intensity = nonbank_direct['total_info_covenants'].value_counts().sort_index()
    bank_direct_intensity = bank_direct['total_info_covenants'].value_counts().sort_index()
    
    x_intensity = np.arange(4)
    nonbank_direct_counts = [nonbank_direct_intensity.get(i, 0) for i in range(4)]
    bank_direct_counts = [bank_direct_intensity.get(i, 0) for i in range(4)]
    
    # Normalize to percentages
    nonbank_direct_total = sum(nonbank_direct_counts)
    bank_direct_total = sum(bank_direct_counts)
    nonbank_direct_pct = [count/nonbank_direct_total*100 if nonbank_direct_total > 0 else 0 for count in nonbank_direct_counts]
    bank_direct_pct = [count/bank_direct_total*100 if bank_direct_total > 0 else 0 for count in bank_direct_counts]
    
    axes[0, 1].bar(x_intensity - width/2, bank_direct_pct, width, label='Bank Direct', alpha=0.8, color='teal')
    axes[0, 1].bar(x_intensity + width/2, nonbank_direct_pct, width, label='Nonbank Direct', alpha=0.8, color='purple')
    axes[0, 1].set_xlabel('Number of Covenant Types')
    axes[0, 1].set_ylabel('Percentage of Loans')
    axes[0, 1].set_title('Covenant Intensity Distribution')
    axes[0, 1].set_xticks(x_intensity)
    axes[0, 1].set_xticklabels(['0', '1', '2', '3'])
    axes[0, 1].legend()
    axes[0, 1].grid(True, alpha=0.3)
    
    # 3. Time series comparison
    nonbank_direct_yearly = nonbank_direct.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
    bank_direct_yearly = bank_direct.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
    
    # Filter years with sufficient data
    nonbank_direct_yearly = nonbank_direct_yearly[nonbank_direct_yearly['count'] >= 3]
    bank_direct_yearly = bank_direct_yearly[bank_direct_yearly['count'] >= 3]
    
    if len(nonbank_direct_yearly) > 0 and len(bank_direct_yearly) > 0:
        ax_twin = axes[1, 0].twinx()
        line1 = axes[1, 0].plot(bank_direct_yearly['year'], bank_direct_yearly['mean'], 'o-', 
                               label='Bank Direct Avg', color='teal', linewidth=2)
        line2 = axes[1, 0].plot(nonbank_direct_yearly['year'], nonbank_direct_yearly['mean'], 's-', 
                               label='Nonbank Direct Avg', color='purple', linewidth=2)
        line3 = ax_twin.plot(bank_direct_yearly['year'], bank_direct_yearly['count'], '--', 
                            alpha=0.5, label='Bank Direct Count', color='teal')
        line4 = ax_twin.plot(nonbank_direct_yearly['year'], nonbank_direct_yearly['count'], '--', 
                            alpha=0.5, label='Nonbank Direct Count', color='purple')
        
        axes[1, 0].set_xlabel('Year')
        axes[1, 0].set_ylabel('Average Covenants', color='black')
        ax_twin.set_ylabel('Number of Loans', color='gray')
        axes[1, 0].set_title('Covenant Usage Trend Over Time')
        axes[1, 0].grid(True, alpha=0.3)
        
        # Combine legends
        lines = line1 + line2 + line3 + line4
        labels = [l.get_label() for l in lines]
        axes[1, 0].legend(lines, labels, loc='upper left', fontsize=8)
    else:
        axes[1, 0].text(0.5, 0.5, 'Insufficient data for time trend', 
                       transform=axes[1, 0].transAxes, ha='center', va='center')
        axes[1, 0].set_title('Covenant Usage Trend Over Time')
    
    # 4. Statistical significance test results
    test_results = []
    for covenant in covenant_types:
        nonbank_values = nonbank_direct[covenant].values
        bank_values = bank_direct[covenant].values
        
        if len(nonbank_values) > 0 and len(bank_values) > 0:
            t_stat, p_value = stats.ttest_ind(nonbank_values, bank_values)
            test_results.append({
                'Covenant': covenant.replace('_', ' ').title(),
                'T-statistic': t_stat,
                'P-value': p_value,
                'Significant': p_value < 0.05
            })
    
    # Create a simple text summary
    axes[1, 1].text(0.1, 0.8, 'Statistical Tests (T-tests):', fontsize=12, fontweight='bold', transform=axes[1, 1].transAxes)
    
    y_pos = 0.7
    for result in test_results:
        significance = "***" if result['P-value'] < 0.01 else "**" if result['P-value'] < 0.05 else "*" if result['P-value'] < 0.1 else ""
        text = f"{result['Covenant']}: p={result['P-value']:.3f}{significance}"
        axes[1, 1].text(0.1, y_pos, text, fontsize=10, transform=axes[1, 1].transAxes)
        y_pos -= 0.1
    
    # Add summary statistics
    axes[1, 1].text(0.1, y_pos-0.1, "\nSummary Statistics:", fontsize=11, fontweight='bold', transform=axes[1, 1].transAxes)
    y_pos -= 0.15
    
    axes[1, 1].text(0.1, y_pos, f"Nonbank Direct (n={len(nonbank_direct)}):", fontsize=9, transform=axes[1, 1].transAxes)
    y_pos -= 0.08
    axes[1, 1].text(0.1, y_pos, f"  Avg Covenants: {nonbank_direct['total_info_covenants'].mean():.3f}", fontsize=9, transform=axes[1, 1].transAxes)
    y_pos -= 0.08
    
    axes[1, 1].text(0.1, y_pos, f"Bank Direct (n={len(bank_direct)}):", fontsize=9, transform=axes[1, 1].transAxes)
    y_pos -= 0.08
    axes[1, 1].text(0.1, y_pos, f"  Avg Covenants: {bank_direct['total_info_covenants'].mean():.3f}", fontsize=9, transform=axes[1, 1].transAxes)
    
    axes[1, 1].text(0.1, y_pos-0.15, "*** p<0.01, ** p<0.05, * p<0.1", fontsize=8, transform=axes[1, 1].transAxes)
    axes[1, 1].set_title('Statistical Significance Tests')
    axes[1, 1].axis('off')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6d_InfoCovenants_Matched_NonbankDirect_vs_BankDirect.png", dpi=300, bbox_inches='tight')
    plt.close()
    
    # Print summary statistics
    print(f"\nSummary Statistics - Nonbank Direct vs Bank Direct:")
    print(f"Nonbank Direct (n={len(nonbank_direct)}):")
    print(f"  Monthly FS: {nonbank_direct['monthly_fs'].mean():.3f}")
    print(f"  Projected FS: {nonbank_direct['projected_fs'].mean():.3f}")
    print(f"  Lender Meeting: {nonbank_direct['lender_meeting'].mean():.3f}")
    print(f"  Avg Total Covenants: {nonbank_direct['total_info_covenants'].mean():.3f}")
    print(f"\nBank Direct (n={len(bank_direct)}):")
    print(f"  Monthly FS: {bank_direct['monthly_fs'].mean():.3f}")
    print(f"  Projected FS: {bank_direct['projected_fs'].mean():.3f}")
    print(f"  Lender Meeting: {bank_direct['lender_meeting'].mean():.3f}")
    print(f"  Avg Total Covenants: {bank_direct['total_info_covenants'].mean():.3f}")


def main():
    print("=" * 80)
    print("6d_DescribeInfoCovMatched.py - Analyze Information Covenants for Matched Samples")
    print("=" * 80)

    # Set up paths
    script_dir = Path(__file__).parent
    direct_matched_file = script_dir / ".." / "Data" / "Intermediate" / "5d_MatchedPanelLoans_Direct.csv"
    panel_file = script_dir / ".." / "Data" / "Intermediate" / "6b_PanelWithInfoCovenants.csv"
    fig_dir = ensure_fig_dir()

    # Check if panel file exists
    if not panel_file.exists():
        print(f"Error: Panel file not found: {panel_file}")
        print("Please run 6b_MergeInfoCov.py first to generate the panel with covenants.")
        return

    # ========================================================================
    # Analyze Direct vs Non-Direct Matched Sample
    # ========================================================================
    if direct_matched_file.exists():
        print("\n" + "="*80)
        print("ANALYZING DIRECT VS NON-DIRECT MATCHED SAMPLE")
        print("="*80)
        
        print(f"Direct matched file: {direct_matched_file}")
        print(f"Panel file: {panel_file}")
        print(f"Output directory: {fig_dir}")

        # Load direct matched sample with covenant data
        df_direct = load_direct_matched_sample_with_covenants(direct_matched_file, panel_file)
        
        # Extract year from deal_active_date
        df_direct['year'] = extract_year_from_date('deal_active_date', df_direct)
        
        # Filter to observations with valid years (no year range restriction)
        df_direct = df_direct[df_direct['year'].notna()]
        print(f"After filtering for valid years: {len(df_direct)} observations")
        
        print(f"Year range: {df_direct['year'].min()} to {df_direct['year'].max()}")
        print(f"Direct loans: {len(df_direct[df_direct['direct'] == 1])}")
        print(f"Non-Direct loans: {len(df_direct[df_direct['direct'] == 0])}")

        # Create visualizations
        print("\nCreating direct matched sample visualizations...")
        create_direct_matched_time_series(df_direct, fig_dir)
        print("  - Matched Direct vs Non-Direct time series plot created")
        
        create_direct_matched_detailed_comparison_plots(df_direct, fig_dir)
        print("  - Matched Direct detailed comparison plots created")
        
        create_direct_matched_pairs_comparison(df_direct, fig_dir)
        print("  - Matched Direct pairs comparison plot created")
        
        create_nonbank_direct_vs_bank_direct_comparison(df_direct, fig_dir)
        print("  - Nonbank Direct vs Bank Direct comparison plot created")
    else:
        print(f"Warning: Direct matched file not found: {direct_matched_file}")
        print("Skipping direct vs non-direct analysis.")

    print(f"\nAll figures saved to: {fig_dir}")
    print("Done.")


if __name__ == "__main__":
    main()
