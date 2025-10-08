#!/usr/bin/env python3
"""
6d_DescribeInfoCovMatched.py

Analyze and visualize information covenant usage for the propensity score matched sample.

Creates time series plots showing:
1. Information covenant usage over time by bank vs nonbank lenders (matched sample)
2. Information covenant usage over time by nonbank lender types (matched sample)
3. Detailed breakdown of covenant types by lender category (matched sample)
4. Comparison of covenant usage between matched nonbank and bank loans

Inputs:
- ../Data/Intermediate/5d_MatchedPanelLoans.csv (matched sample)
- ../Data/Intermediate/6b_PanelWithInfoCovenants.csv (full panel with covenants)

Outputs:
- ../Results/Figures/6d_InfoCovenants_Matched_Bank_vs_Nonbank.png
- ../Results/Figures/6d_InfoCovenants_Matched_NonbankTypes.png
- ../Results/Figures/6d_InfoCovenants_Matched_Detailed.png
- ../Results/Figures/6d_InfoCovenants_Matched_Comparison.png

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


def load_matched_sample_with_covenants(matched_file, panel_file):
    """Load matched sample and merge with covenant data."""
    print("Loading matched sample...")
    df_matched = pd.read_csv(matched_file)
    print(f"  Matched sample: {len(df_matched)} pairs")
    
    print("Loading panel with covenants...")
    df_panel = pd.read_csv(panel_file)
    print(f"  Panel with covenants: {len(df_panel)} observations")
    
    # Get loan IDs from matched sample
    nonbank_loan_ids = df_matched['loan_id_nonbank'].tolist()
    bank_loan_ids = df_matched['loan_id_bank'].tolist()
    all_loan_ids = nonbank_loan_ids + bank_loan_ids
    
    # Filter panel to matched loans only
    df_matched_loans = df_panel[df_panel['loan_id'].isin(all_loan_ids)].copy()
    print(f"  Matched loans with covenant data: {len(df_matched_loans)}")
    
    # Create a mapping from loan_id to pair information
    pair_info = {}
    for _, row in df_matched.iterrows():
        pair_info[row['loan_id_nonbank']] = {
            'ff12': row['ff12'],
            'term_loan': row['term_loan'],
            'is_nonbank_pair': True
        }
        pair_info[row['loan_id_bank']] = {
            'ff12': row['ff12'],
            'term_loan': row['term_loan'],
            'is_nonbank_pair': False
        }
    
    # Add pair information to matched loans
    df_matched_loans['ff12'] = df_matched_loans['loan_id'].map(lambda x: pair_info.get(x, {}).get('ff12'))
    df_matched_loans['term_loan'] = df_matched_loans['loan_id'].map(lambda x: pair_info.get(x, {}).get('term_loan'))
    df_matched_loans['is_nonbank_pair'] = df_matched_loans['loan_id'].map(lambda x: pair_info.get(x, {}).get('is_nonbank_pair', False))
    
    return df_matched_loans


def create_matched_time_series_bank_vs_nonbank(df, fig_dir):
    """Create time series plots comparing bank vs nonbank covenant usage in matched sample."""
    
    # Calculate annual covenant usage rates
    bank_data = df[df['nonbank_lender'] == 0].copy()
    nonbank_data = df[df['nonbank_lender'] == 1].copy()
    
    # Group by year and calculate rates
    bank_yearly = bank_data.groupby('year').agg({
        'monthly_fs': 'mean',
        'projected_fs': 'mean', 
        'lender_meeting': 'mean',
        'total_info_covenants': ['mean', 'count']
    }).reset_index()
    
    nonbank_yearly = nonbank_data.groupby('year').agg({
        'monthly_fs': 'mean',
        'projected_fs': 'mean',
        'lender_meeting': 'mean', 
        'total_info_covenants': ['mean', 'count']
    }).reset_index()
    
    # Flatten column names
    bank_yearly.columns = ['year', 'monthly_fs_rate', 'projected_fs_rate', 'meeting_rate', 'avg_covenants', 'count']
    nonbank_yearly.columns = ['year', 'monthly_fs_rate', 'projected_fs_rate', 'meeting_rate', 'avg_covenants', 'count']
    
    # Filter years with sufficient data (at least 5 observations for matched sample)
    bank_yearly = bank_yearly[bank_yearly['count'] >= 5]
    nonbank_yearly = nonbank_yearly[nonbank_yearly['count'] >= 5]
    
    # Create subplot
    fig = make_subplots(
        rows=2, cols=2,
        subplot_titles=('Monthly FS Requirements', 'Projected FS Requirements', 
                       'Lender Meeting Requirements', 'Average Total Covenants'),
        vertical_spacing=0.12
    )
    
    # Plot 1: Monthly FS
    fig.add_trace(
        go.Scatter(x=bank_yearly['year'], y=bank_yearly['monthly_fs_rate'], 
                  name='Bank (Matched)', line=dict(color='blue'), mode='lines+markers'),
        row=1, col=1
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['monthly_fs_rate'],
                  name='Nonbank (Matched)', line=dict(color='red'), mode='lines+markers'),
        row=1, col=1
    )
    
    # Plot 2: Projected FS
    fig.add_trace(
        go.Scatter(x=bank_yearly['year'], y=bank_yearly['projected_fs_rate'],
                  name='Bank (Matched)', line=dict(color='blue'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['projected_fs_rate'],
                  name='Nonbank (Matched)', line=dict(color='red'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    
    # Plot 3: Lender Meetings
    fig.add_trace(
        go.Scatter(x=bank_yearly['year'], y=bank_yearly['meeting_rate'],
                  name='Bank (Matched)', line=dict(color='blue'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['meeting_rate'],
                  name='Nonbank (Matched)', line=dict(color='red'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    
    # Plot 4: Average Total Covenants
    fig.add_trace(
        go.Scatter(x=bank_yearly['year'], y=bank_yearly['avg_covenants'],
                  name='Bank (Matched)', line=dict(color='blue'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['avg_covenants'],
                  name='Nonbank (Matched)', line=dict(color='red'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    
    fig.update_layout(
        title='Information Covenant Usage Over Time: Matched Bank vs Nonbank Lenders',
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
    pio.write_image(fig, str(fig_dir / "6d_InfoCovenants_Matched_Bank_vs_Nonbank.png"), 
                   width=1200, height=800, scale=2)


def create_matched_time_series_nonbank_types(df, fig_dir):
    """Create time series plots for different nonbank lender types in matched sample."""
    
    # Get nonbank data only
    nonbank_data = df[df['nonbank_lender'] == 1].copy()
    
    if 'lender_type' not in nonbank_data.columns:
        print("Warning: lender_type column not found. Cannot create nonbank types analysis.")
        return
    
    # Get top nonbank lender types by count
    lender_counts = nonbank_data['lender_type'].value_counts()
    top_lender_types = lender_counts.head(6).index.tolist()  # Top 6 for readability
    
    print(f"Top nonbank lender types in matched sample: {top_lender_types}")
    print(f"Lender type counts: {lender_counts.head(6).to_dict()}")
    
    # Filter to top lender types
    nonbank_filtered = nonbank_data[nonbank_data['lender_type'].isin(top_lender_types)]
    
    # Create subplot
    fig = make_subplots(
        rows=2, cols=2,
        subplot_titles=('Monthly FS Requirements', 'Projected FS Requirements',
                       'Lender Meeting Requirements', 'Average Total Covenants'),
        vertical_spacing=0.12
    )
    
    colors = ['red', 'orange', 'green', 'purple', 'brown', 'pink']
    
    for i, lender_type in enumerate(top_lender_types):
        lender_data = nonbank_filtered[nonbank_filtered['lender_type'] == lender_type]
        
        # Group by year
        yearly_data = lender_data.groupby('year').agg({
            'monthly_fs': 'mean',
            'projected_fs': 'mean',
            'lender_meeting': 'mean',
            'total_info_covenants': ['mean', 'count']
        }).reset_index()
        
        # Flatten column names
        yearly_data.columns = ['year', 'monthly_fs_rate', 'projected_fs_rate', 'meeting_rate', 'avg_covenants', 'count']
        
        # Filter years with sufficient data (lower threshold for matched sample)
        yearly_data = yearly_data[yearly_data['count'] >= 3]
        
        if len(yearly_data) > 0:
            color = colors[i % len(colors)]
            show_legend = True  # Show legend for all traces
            
            # Plot 1: Monthly FS
            fig.add_trace(
                go.Scatter(x=yearly_data['year'], y=yearly_data['monthly_fs_rate'],
                          name=lender_type, line=dict(color=color), mode='lines+markers', showlegend=show_legend),
                row=1, col=1
            )
            
            # Plot 2: Projected FS
            fig.add_trace(
                go.Scatter(x=yearly_data['year'], y=yearly_data['projected_fs_rate'],
                          name=lender_type, line=dict(color=color), mode='lines+markers', showlegend=False),
                row=1, col=2
            )
            
            # Plot 3: Lender Meetings
            fig.add_trace(
                go.Scatter(x=yearly_data['year'], y=yearly_data['meeting_rate'],
                          name=lender_type, line=dict(color=color), mode='lines+markers', showlegend=False),
                row=2, col=1
            )
            
            # Plot 4: Average Total Covenants
            fig.add_trace(
                go.Scatter(x=yearly_data['year'], y=yearly_data['avg_covenants'],
                          name=lender_type, line=dict(color=color), mode='lines+markers', showlegend=False),
                row=2, col=2
            )
    
    fig.update_layout(
        title='Information Covenant Usage Over Time: Matched Nonbank Lender Types',
        height=800,
        title_x=0.5,
        legend=dict(
            orientation="v",
            yanchor="top",
            y=1,
            xanchor="left",
            x=1.02
        )
    )
    
    # Update y-axis labels
    fig.update_yaxes(title_text="Usage Rate", row=1, col=1)
    fig.update_yaxes(title_text="Usage Rate", row=1, col=2)
    fig.update_yaxes(title_text="Usage Rate", row=2, col=1)
    fig.update_yaxes(title_text="Average Count", row=2, col=2)
    
    fig.update_xaxes(title_text="Year", row=2, col=1)
    fig.update_xaxes(title_text="Year", row=2, col=2)
    
    # Save plot
    pio.write_image(fig, str(fig_dir / "6d_InfoCovenants_Matched_NonbankTypes.png"),
                   width=1200, height=800, scale=2)


def create_matched_detailed_comparison_plots(df, fig_dir):
    """Create detailed comparison plots for matched sample using matplotlib/seaborn."""
    
    # Set style
    plt.style.use('default')
    sns.set_palette("husl")
    
    # Create figure with subplots
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('Information Covenant Usage: Matched Sample Analysis', fontsize=16, y=0.98)
    
    # 1. Covenant usage by lender type (bank vs nonbank)
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS', 'Projected FS', 'Lender Meeting']
    
    bank_rates = []
    nonbank_rates = []
    
    for covenant in covenant_types:
        bank_rate = df[df['nonbank_lender'] == 0][covenant].mean()
        nonbank_rate = df[df['nonbank_lender'] == 1][covenant].mean()
        bank_rates.append(bank_rate)
        nonbank_rates.append(nonbank_rate)
    
    x = np.arange(len(covenant_names))
    width = 0.35
    
    axes[0, 0].bar(x - width/2, bank_rates, width, label='Bank (Matched)', alpha=0.8)
    axes[0, 0].bar(x + width/2, nonbank_rates, width, label='Nonbank (Matched)', alpha=0.8)
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage: Matched Bank vs Nonbank')
    axes[0, 0].set_xticks(x)
    axes[0, 0].set_xticklabels(covenant_names, rotation=45)
    axes[0, 0].legend()
    axes[0, 0].grid(True, alpha=0.3)
    
    # 2. Covenant intensity distribution
    bank_intensity = df[df['nonbank_lender'] == 0]['total_info_covenants'].value_counts().sort_index()
    nonbank_intensity = df[df['nonbank_lender'] == 1]['total_info_covenants'].value_counts().sort_index()
    
    x_intensity = np.arange(4)
    bank_counts = [bank_intensity.get(i, 0) for i in range(4)]
    nonbank_counts = [nonbank_intensity.get(i, 0) for i in range(4)]
    
    # Normalize to percentages
    bank_total = sum(bank_counts)
    nonbank_total = sum(nonbank_counts)
    bank_pct = [count/bank_total*100 for count in bank_counts]
    nonbank_pct = [count/nonbank_total*100 for count in nonbank_counts]
    
    axes[0, 1].bar(x_intensity - width/2, bank_pct, width, label='Bank (Matched)', alpha=0.8)
    axes[0, 1].bar(x_intensity + width/2, nonbank_pct, width, label='Nonbank (Matched)', alpha=0.8)
    axes[0, 1].set_xlabel('Number of Covenant Types')
    axes[0, 1].set_ylabel('Percentage of Loans')
    axes[0, 1].set_title('Covenant Intensity Distribution (Matched)')
    axes[0, 1].set_xticks(x_intensity)
    axes[0, 1].set_xticklabels(['0', '1', '2', '3'])
    axes[0, 1].legend()
    axes[0, 1].grid(True, alpha=0.3)
    
    # 3. Nonbank lender types comparison (if available)
    if 'lender_type' in df.columns:
        nonbank_data = df[df['nonbank_lender'] == 1]
        lender_counts = nonbank_data['lender_type'].value_counts().head(8)
        
        # Calculate average covenants per lender type
        lender_avg_covenants = nonbank_data.groupby('lender_type')['total_info_covenants'].mean().sort_values(ascending=False)
        
        axes[1, 0].barh(range(len(lender_avg_covenants)), lender_avg_covenants.values, alpha=0.8)
        axes[1, 0].set_yticks(range(len(lender_avg_covenants)))
        axes[1, 0].set_yticklabels([name[:20] + '...' if len(name) > 20 else name 
                                   for name in lender_avg_covenants.index], fontsize=8)
        axes[1, 0].set_xlabel('Average Number of Covenants')
        axes[1, 0].set_title('Average Covenants by Nonbank Lender Type (Matched)')
        axes[1, 0].grid(True, alpha=0.3)
    
    # 4. Time trend of overall covenant usage
    yearly_trend = df.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
    yearly_trend = yearly_trend[yearly_trend['count'] >= 5]  # Filter years with sufficient data
    
    ax_twin = axes[1, 1].twinx()
    line1 = axes[1, 1].plot(yearly_trend['year'], yearly_trend['mean'], 'b-o', label='Avg Covenants')
    line2 = ax_twin.plot(yearly_trend['year'], yearly_trend['count'], 'r-s', label='Loan Count')
    
    axes[1, 1].set_xlabel('Year')
    axes[1, 1].set_ylabel('Average Covenants', color='b')
    ax_twin.set_ylabel('Number of Loans', color='r')
    axes[1, 1].set_title('Covenant Usage Trend Over Time (Matched)')
    axes[1, 1].grid(True, alpha=0.3)
    
    # Combine legends
    lines = line1 + line2
    labels = [l.get_label() for l in lines]
    axes[1, 1].legend(lines, labels, loc='upper left')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6d_InfoCovenants_Matched_Detailed.png", dpi=300, bbox_inches='tight')
    plt.close()


def create_matched_pairs_comparison(df, fig_dir):
    """Create comparison plots showing matched pairs side by side."""
    
    # Create figure
    fig, axes = plt.subplots(2, 2, figsize=(15, 10))
    fig.suptitle('Matched Pairs: Nonbank vs Bank Covenant Comparison', fontsize=16, y=0.98)
    
    # Get matched pairs data
    nonbank_pairs = df[df['is_nonbank_pair'] == True].copy()
    bank_pairs = df[df['is_nonbank_pair'] == False].copy()
    
    # Ensure we have the same number of pairs
    min_pairs = min(len(nonbank_pairs), len(bank_pairs))
    nonbank_pairs = nonbank_pairs.head(min_pairs)
    bank_pairs = bank_pairs.head(min_pairs)
    
    # 1. Covenant usage comparison
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS', 'Projected FS', 'Lender Meeting']
    
    nonbank_rates = [nonbank_pairs[covenant].mean() for covenant in covenant_types]
    bank_rates = [bank_pairs[covenant].mean() for covenant in covenant_types]
    
    x = np.arange(len(covenant_names))
    width = 0.35
    
    axes[0, 0].bar(x - width/2, bank_rates, width, label='Bank Pairs', alpha=0.8, color='blue')
    axes[0, 0].bar(x + width/2, nonbank_rates, width, label='Nonbank Pairs', alpha=0.8, color='red')
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage: Matched Pairs')
    axes[0, 0].set_xticks(x)
    axes[0, 0].set_xticklabels(covenant_names, rotation=45)
    axes[0, 0].legend()
    axes[0, 0].grid(True, alpha=0.3)
    
    # 2. Average total covenants
    avg_covenants = [nonbank_pairs['total_info_covenants'].mean(), bank_pairs['total_info_covenants'].mean()]
    axes[0, 1].bar(['Bank Pairs', 'Nonbank Pairs'], avg_covenants, alpha=0.8, color=['blue', 'red'])
    axes[0, 1].set_ylabel('Average Total Covenants')
    axes[0, 1].set_title('Average Covenant Count: Matched Pairs')
    axes[0, 1].grid(True, alpha=0.3)
    
    # 3. Covenant intensity distribution comparison
    nonbank_intensity = nonbank_pairs['total_info_covenants'].value_counts().sort_index()
    bank_intensity = bank_pairs['total_info_covenants'].value_counts().sort_index()
    
    x_intensity = np.arange(4)
    nonbank_counts = [nonbank_intensity.get(i, 0) for i in range(4)]
    bank_counts = [bank_intensity.get(i, 0) for i in range(4)]
    
    # Normalize to percentages
    nonbank_total = sum(nonbank_counts)
    bank_total = sum(bank_counts)
    nonbank_pct = [count/nonbank_total*100 for count in nonbank_counts]
    bank_pct = [count/bank_total*100 for count in bank_counts]
    
    axes[1, 0].bar(x_intensity - width/2, bank_pct, width, label='Bank Pairs', alpha=0.8, color='blue')
    axes[1, 0].bar(x_intensity + width/2, nonbank_pct, width, label='Nonbank Pairs', alpha=0.8, color='red')
    axes[1, 0].set_xlabel('Number of Covenant Types')
    axes[1, 0].set_ylabel('Percentage of Pairs')
    axes[1, 0].set_title('Covenant Intensity Distribution: Matched Pairs')
    axes[1, 0].set_xticks(x_intensity)
    axes[1, 0].set_xticklabels(['0', '1', '2', '3'])
    axes[1, 0].legend()
    axes[1, 0].grid(True, alpha=0.3)
    
    # 4. Statistical significance test results
    
    # Perform t-tests for each covenant type
    test_results = []
    for covenant in covenant_types:
        nonbank_values = nonbank_pairs[covenant].values
        bank_values = bank_pairs[covenant].values
        
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
    
    axes[1, 1].text(0.1, y_pos-0.1, "*** p<0.01, ** p<0.05, * p<0.1", fontsize=8, transform=axes[1, 1].transAxes)
    axes[1, 1].set_title('Statistical Significance Tests')
    axes[1, 1].axis('off')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6d_InfoCovenants_Matched_Comparison.png", dpi=300, bbox_inches='tight')
    plt.close()


def create_bdc_private_credit_comparison(df, fig_dir):
    """Create comparison plots between BDCs, Private Credit, and Banks."""
    
    # Filter data for the three groups
    bank_data = df[df['nonbank_lender'] == 0].copy()
    
    # Identify BDCs and Private Credit lenders
    bdc_data = df[(df['nonbank_lender'] == 1) & 
                  (df['lender_type'] == 'Business Development Companies (BDCs)')].copy()
    
    private_credit_data = df[(df['nonbank_lender'] == 1) & 
                            (df['lender_type'] == 'Specialty Direct Lenders / Private Credit')].copy()
    
    print(f"Comparison groups:")
    print(f"  Banks: {len(bank_data)}")
    print(f"  BDCs: {len(bdc_data)}")
    print(f"  Private Credit: {len(private_credit_data)}")
    
    if len(bdc_data) == 0 and len(private_credit_data) == 0:
        print("Warning: No BDC or Private Credit data found for comparison")
        return
    
    # Create figure with subplots
    fig, axes = plt.subplots(2, 3, figsize=(18, 12))
    fig.suptitle('Information Covenants: BDCs and Private Credit vs Banks', fontsize=16, y=0.98)
    
    # Define groups and colors
    groups = {'Bank': bank_data, 'BDC': bdc_data, 'Private Credit': private_credit_data}
    colors = {'Bank': 'blue', 'BDC': 'green', 'Private Credit': 'red'}
    
    # 1. Covenant usage comparison
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS', 'Projected FS', 'Lender Meeting']
    
    x = np.arange(len(covenant_names))
    width = 0.25
    
    for i, (group_name, group_data) in enumerate(groups.items()):
        if len(group_data) > 0:
            rates = [group_data[covenant].mean() for covenant in covenant_types]
            axes[0, 0].bar(x + i*width, rates, width, label=group_name, 
                          alpha=0.8, color=colors[group_name])
    
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage by Lender Type')
    axes[0, 0].set_xticks(x + width)
    axes[0, 0].set_xticklabels(covenant_names, rotation=45)
    axes[0, 0].legend()
    axes[0, 0].grid(True, alpha=0.3)
    
    # 2. Average total covenants
    avg_covenants = []
    group_labels = []
    for group_name, group_data in groups.items():
        if len(group_data) > 0:
            avg_covenants.append(group_data['total_info_covenants'].mean())
            group_labels.append(f"{group_name}\n(n={len(group_data)})")
    
    # Create color mapping
    color_mapping = []
    for group in group_labels:
        if 'Bank' in group:
            color_mapping.append(colors['Bank'])
        elif 'BDC' in group:
            color_mapping.append(colors['BDC'])
        elif 'Private Credit' in group:
            color_mapping.append(colors['Private Credit'])
        else:
            color_mapping.append('gray')
    
    axes[0, 1].bar(group_labels, avg_covenants, alpha=0.8, color=color_mapping)
    axes[0, 1].set_ylabel('Average Total Covenants')
    axes[0, 1].set_title('Average Covenant Count by Lender Type')
    axes[0, 1].grid(True, alpha=0.3)
    
    # 3. Covenant intensity distribution
    for i, (group_name, group_data) in enumerate(groups.items()):
        if len(group_data) > 0:
            intensity = group_data['total_info_covenants'].value_counts().sort_index()
            counts = [intensity.get(j, 0) for j in range(4)]
            total = sum(counts)
            pct = [count/total*100 for count in counts]
            
            x_intensity = np.arange(4) + i*0.25
            axes[0, 2].bar(x_intensity, pct, width=0.25, label=group_name, 
                          alpha=0.8, color=colors[group_name])
    
    axes[0, 2].set_xlabel('Number of Covenant Types')
    axes[0, 2].set_ylabel('Percentage of Loans')
    axes[0, 2].set_title('Covenant Intensity Distribution')
    axes[0, 2].set_xticks(np.arange(4) + 0.25)
    axes[0, 2].set_xticklabels(['0', '1', '2', '3'])
    axes[0, 2].legend()
    axes[0, 2].grid(True, alpha=0.3)
    
    # 4. Time series for BDCs vs Banks
    if len(bdc_data) > 0:
        bank_yearly = bank_data.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
        bdc_yearly = bdc_data.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
        
        # Filter years with sufficient data
        bank_yearly = bank_yearly[bank_yearly['count'] >= 3]
        bdc_yearly = bdc_yearly[bdc_yearly['count'] >= 3]
        
        if len(bank_yearly) > 0 and len(bdc_yearly) > 0:
            axes[1, 0].plot(bank_yearly['year'], bank_yearly['mean'], 'o-', 
                           label='Bank', color=colors['Bank'], linewidth=2)
            axes[1, 0].plot(bdc_yearly['year'], bdc_yearly['mean'], 's-', 
                           label='BDC', color=colors['BDC'], linewidth=2)
            axes[1, 0].set_xlabel('Year')
            axes[1, 0].set_ylabel('Average Covenants')
            axes[1, 0].set_title('Covenant Trends: BDCs vs Banks')
            axes[1, 0].legend()
            axes[1, 0].grid(True, alpha=0.3)
    
    # 5. Time series for Private Credit vs Banks
    if len(private_credit_data) > 0:
        bank_yearly = bank_data.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
        pc_yearly = private_credit_data.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
        
        # Filter years with sufficient data
        bank_yearly = bank_yearly[bank_yearly['count'] >= 3]
        pc_yearly = pc_yearly[pc_yearly['count'] >= 3]
        
        if len(bank_yearly) > 0 and len(pc_yearly) > 0:
            axes[1, 1].plot(bank_yearly['year'], bank_yearly['mean'], 'o-', 
                           label='Bank', color=colors['Bank'], linewidth=2)
            axes[1, 1].plot(pc_yearly['year'], pc_yearly['mean'], '^-', 
                           label='Private Credit', color=colors['Private Credit'], linewidth=2)
            axes[1, 1].set_xlabel('Year')
            axes[1, 1].set_ylabel('Average Covenants')
            axes[1, 1].set_title('Covenant Trends: Private Credit vs Banks')
            axes[1, 1].legend()
            axes[1, 1].grid(True, alpha=0.3)
    
    # 6. Statistical significance tests
    axes[1, 2].text(0.1, 0.9, 'Statistical Tests (T-tests):', fontsize=12, fontweight='bold', 
                    transform=axes[1, 2].transAxes)
    
    y_pos = 0.8
    test_results = []
    
    # Test BDCs vs Banks
    if len(bdc_data) > 0 and len(bank_data) > 0:
        for covenant in covenant_types:
            bdc_values = bdc_data[covenant].values
            bank_values = bank_data[covenant].values
            
            if len(bdc_values) > 0 and len(bank_values) > 0:
                t_stat, p_value = stats.ttest_ind(bdc_values, bank_values)
                significance = "***" if p_value < 0.01 else "**" if p_value < 0.05 else "*" if p_value < 0.1 else ""
                text = f"BDC vs Bank {covenant.replace('_', ' ').title()}: p={p_value:.3f}{significance}"
                axes[1, 2].text(0.1, y_pos, text, fontsize=9, transform=axes[1, 2].transAxes)
                y_pos -= 0.08
                
                test_results.append({
                    'Comparison': f'BDC vs Bank {covenant.replace("_", " ").title()}',
                    'P-value': p_value,
                    'Significant': p_value < 0.05
                })
    
    # Test Private Credit vs Banks
    if len(private_credit_data) > 0 and len(bank_data) > 0:
        for covenant in covenant_types:
            pc_values = private_credit_data[covenant].values
            bank_values = bank_data[covenant].values
            
            if len(pc_values) > 0 and len(bank_values) > 0:
                t_stat, p_value = stats.ttest_ind(pc_values, bank_values)
                significance = "***" if p_value < 0.01 else "**" if p_value < 0.05 else "*" if p_value < 0.1 else ""
                text = f"PC vs Bank {covenant.replace('_', ' ').title()}: p={p_value:.3f}{significance}"
                axes[1, 2].text(0.1, y_pos, text, fontsize=9, transform=axes[1, 2].transAxes)
                y_pos -= 0.08
                
                test_results.append({
                    'Comparison': f'PC vs Bank {covenant.replace("_", " ").title()}',
                    'P-value': p_value,
                    'Significant': p_value < 0.05
                })
    
    axes[1, 2].text(0.1, y_pos-0.1, "*** p<0.01, ** p<0.05, * p<0.1", fontsize=8, 
                    transform=axes[1, 2].transAxes)
    axes[1, 2].set_title('Statistical Significance Tests')
    axes[1, 2].axis('off')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6d_InfoCovenants_BDC_PrivateCredit_vs_Banks.png", 
                dpi=300, bbox_inches='tight')
    plt.close()
    
    # Print summary statistics
    print(f"\nSummary Statistics:")
    for group_name, group_data in groups.items():
        if len(group_data) > 0:
            print(f"\n{group_name} (n={len(group_data)}):")
            print(f"  Monthly FS: {group_data['monthly_fs'].mean():.3f}")
            print(f"  Projected FS: {group_data['projected_fs'].mean():.3f}")
            print(f"  Lender Meeting: {group_data['lender_meeting'].mean():.3f}")
            print(f"  Avg Total Covenants: {group_data['total_info_covenants'].mean():.3f}")


def create_detailed_covenant_timeseries(df, fig_dir):
    """Create detailed time series plots for each covenant type: Banks vs BDCs vs Private Credit."""
    
    # Filter data for the three groups
    bank_data = df[df['nonbank_lender'] == 0].copy()
    bdc_data = df[(df['nonbank_lender'] == 1) & 
                  (df['lender_type'] == 'Business Development Companies (BDCs)')].copy()
    private_credit_data = df[(df['nonbank_lender'] == 1) & 
                            (df['lender_type'] == 'Specialty Direct Lenders / Private Credit')].copy()
    
    print(f"Detailed time series groups:")
    print(f"  Banks: {len(bank_data)}")
    print(f"  BDCs: {len(bdc_data)}")
    print(f"  Private Credit: {len(private_credit_data)}")
    
    if len(bdc_data) == 0 and len(private_credit_data) == 0:
        print("Warning: No BDC or Private Credit data found for detailed time series")
        return
    
    # Define covenant types and their display names
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS Requirements', 'Projected FS Requirements', 'Lender Meeting Requirements']
    
    # Create figure with subplots for each covenant
    fig = make_subplots(
        rows=1, cols=3,
        subplot_titles=covenant_names,
        vertical_spacing=0.15
    )
    
    # Define colors and line styles
    colors = {'Bank': 'blue', 'BDC': 'green', 'Private Credit': 'red'}
    line_styles = {'Bank': 'solid', 'BDC': 'dash', 'Private Credit': 'dot'}
    markers = {'Bank': 'circle', 'BDC': 'square', 'Private Credit': 'triangle-up'}
    
    # Process each covenant type
    for i, (covenant, covenant_name) in enumerate(zip(covenant_types, covenant_names), 1):
        col = i
        
        # Calculate yearly rates for each group
        groups_data = {}
        
        for group_name, group_data in [('Bank', bank_data), ('BDC', bdc_data), ('Private Credit', private_credit_data)]:
            if len(group_data) > 0:
                yearly_data = group_data.groupby('year')[covenant].agg(['mean', 'count']).reset_index()
                yearly_data.columns = ['year', 'rate', 'count']
                
                # Filter years with sufficient data (at least 3 observations)
                yearly_data = yearly_data[yearly_data['count'] >= 3]
                
                if len(yearly_data) > 0:
                    groups_data[group_name] = yearly_data
        
        # Plot each group for this covenant
        for group_name, yearly_data in groups_data.items():
            fig.add_trace(
                go.Scatter(
                    x=yearly_data['year'], 
                    y=yearly_data['rate'],
                    name=group_name,
                    line=dict(color=colors[group_name], dash=line_styles[group_name], width=3),
                    marker=dict(symbol=markers[group_name], size=8),
                    mode='lines+markers',
                    showlegend=(i == 1),  # Only show legend for first subplot
                    hovertemplate=f'<b>{group_name}</b><br>' +
                                 f'Year: %{{x}}<br>' +
                                 f'{covenant_name}: %{{y:.3f}}<br>' +
                                 '<extra></extra>'
                ),
                row=1, col=col
            )
    
    # Update layout
    fig.update_layout(
        title='Information Covenant Usage Over Time: Banks vs BDCs vs Private Credit',
        height=500,
        width=1500,
        title_x=0.5,
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="right",
            x=1
        )
    )
    
    # Update y-axis labels
    for i in range(1, 4):
        fig.update_yaxes(title_text="Usage Rate", row=1, col=i)
        fig.update_yaxes(range=[0, 1], row=1, col=i)  # Set consistent y-axis range
    
    # Update x-axis labels
    fig.update_xaxes(title_text="Year", row=1, col=2)
    
    # Add grid
    for i in range(1, 4):
        fig.update_xaxes(showgrid=True, gridwidth=1, gridcolor='lightgray', row=1, col=i)
        fig.update_yaxes(showgrid=True, gridwidth=1, gridcolor='lightgray', row=1, col=i)
    
    # Save the plot
    pio.write_image(fig, str(fig_dir / "6d_InfoCovenants_Detailed_TimeSeries_Banks_vs_BDCs_vs_PrivateCredit.png"), 
                   width=1500, height=500, scale=2)
    
    print(f"Detailed covenant time series plot created")


def main():
    print("=" * 80)
    print("6d_DescribeInfoCov.py - Analyze Information Covenants for Matched Sample")
    print("=" * 80)

    # Set up paths
    script_dir = Path(__file__).parent
    matched_file = script_dir / ".." / "Data" / "Intermediate" / "5d_MatchedPanelLoans.csv"
    panel_file = script_dir / ".." / "Data" / "Intermediate" / "6b_PanelWithInfoCovenants.csv"
    fig_dir = ensure_fig_dir()

    # Check if input files exist
    if not matched_file.exists():
        print(f"Error: Matched file not found: {matched_file}")
        print("Please run 5d_MatchedPanelLoans.py first to generate the matched sample.")
        return
        
    if not panel_file.exists():
        print(f"Error: Panel file not found: {panel_file}")
        print("Please run 6b_AggregateInfoCov.py first to generate the panel with covenants.")
        return

    print(f"Matched file: {matched_file}")
    print(f"Panel file: {panel_file}")
    print(f"Output directory: {fig_dir}")

    # Load matched sample with covenant data
    df = load_matched_sample_with_covenants(matched_file, panel_file)
    
    # Extract year from deal_active_date
    df['year'] = extract_year_from_date('deal_active_date', df)
    
    # Filter to observations with valid years
    df = df[df['year'].notna() & (df['year'] >= 2010) & (df['year'] <= 2023)]
    print(f"After filtering by year: {len(df)} observations")
    
    print(f"Year range: {df['year'].min()} to {df['year'].max()}")
    print(f"Bank loans: {len(df[df['nonbank_lender'] == 0])}")
    print(f"Nonbank loans: {len(df[df['nonbank_lender'] == 1])}")

    # Create visualizations
    print("\nCreating matched sample visualizations...")
    create_matched_time_series_bank_vs_nonbank(df, fig_dir)
    print("  - Matched Bank vs Nonbank time series plot created")
    
    create_matched_time_series_nonbank_types(df, fig_dir)
    print("  - Matched Nonbank types time series plot created")
    
    create_matched_detailed_comparison_plots(df, fig_dir)
    print("  - Matched detailed comparison plots created")
    
    create_matched_pairs_comparison(df, fig_dir)
    print("  - Matched pairs comparison plot created")
    
    create_bdc_private_credit_comparison(df, fig_dir)
    print("  - BDC and Private Credit vs Banks comparison plot created")
    
    create_detailed_covenant_timeseries(df, fig_dir)
    print("  - Detailed covenant time series plot created")

    print(f"\nAll figures saved to: {fig_dir}")
    print("Done.")


if __name__ == "__main__":
    main()
