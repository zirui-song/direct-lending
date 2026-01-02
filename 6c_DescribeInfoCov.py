#!/usr/bin/env python3
"""
6c_DescribeInfoCov.py

Analyze and visualize information covenant usage for direct vs non-direct loans.

Creates plots showing:
1. Information covenant usage over time by direct vs non-direct loans
2. Detailed breakdown of covenant types and loan characteristics for direct bank vs non-direct bank loans

Inputs:
- ../Data/Intermediate/6b_PanelWithInfoCovenants.csv

Outputs:
- ../Results/Figures/6c_InfoCovenants_TimeSeries_Direct_vs_NonDirect.png
- ../Results/Figures/6c_InfoCovenants_Detailed_DirectBank_vs_NonDirectBank.png

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


def create_time_series_direct_vs_nondirect(df, fig_dir):
    """Create time series plots comparing direct vs non-direct covenant usage."""
    
    # Check if direct_from_text column exists
    if 'direct_from_text' not in df.columns:
        print("Warning: direct_from_text column not found. Skipping direct vs non-direct analysis.")
        return
    
    # Create direct indicator (1 if direct, 0 otherwise)
    df['direct'] = df['direct_from_text'].fillna(0).astype(int)
    
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
    
    # Filter years with sufficient data (at least 10 observations)
    direct_yearly = direct_yearly[direct_yearly['count'] >= 10]
    nondirect_yearly = nondirect_yearly[nondirect_yearly['count'] >= 10]
    
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
                  name='Direct', line=dict(color='green'), mode='lines+markers'),
        row=1, col=1
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['monthly_fs_rate'],
                  name='Non-Direct', line=dict(color='orange'), mode='lines+markers'),
        row=1, col=1
    )
    
    # Plot 2: Projected FS
    fig.add_trace(
        go.Scatter(x=direct_yearly['year'], y=direct_yearly['projected_fs_rate'],
                  name='Direct', line=dict(color='green'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['projected_fs_rate'],
                  name='Non-Direct', line=dict(color='orange'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    
    # Plot 3: Lender Meetings
    fig.add_trace(
        go.Scatter(x=direct_yearly['year'], y=direct_yearly['meeting_rate'],
                  name='Direct', line=dict(color='green'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['meeting_rate'],
                  name='Non-Direct', line=dict(color='orange'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    
    # Plot 4: Average Total Covenants
    fig.add_trace(
        go.Scatter(x=direct_yearly['year'], y=direct_yearly['avg_covenants'],
                  name='Direct', line=dict(color='green'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    fig.add_trace(
        go.Scatter(x=nondirect_yearly['year'], y=nondirect_yearly['avg_covenants'],
                  name='Non-Direct', line=dict(color='orange'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    
    fig.update_layout(
        title='Information Covenant Usage Over Time: Direct vs Non-Direct Loans',
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
    pio.write_image(fig, str(fig_dir / "6c_InfoCovenants_TimeSeries_Direct_vs_NonDirect.png"), 
                   width=1200, height=800, scale=2)


def create_detailed_comparison_direct_bank_vs_nondirect_bank(df, fig_dir):
    """Create detailed comparison plots for direct bank vs non-direct bank loans."""
    
    # Check if required columns exist
    if 'direct_from_text' not in df.columns or 'nonbank_lender' not in df.columns:
        print("Warning: Required columns (direct_from_text or nonbank_lender) not found. Skipping direct bank vs non-direct bank analysis.")
        return
    
    # Create direct indicator
    df['direct'] = df['direct_from_text'].fillna(0).astype(int)
    
    # Filter to bank loans only
    bank_data = df[df['nonbank_lender'] == 0].copy()
    
    if len(bank_data) == 0:
        print("Warning: No bank loans found. Skipping direct bank vs non-direct bank analysis.")
        return
    
    # Split into direct bank and non-direct bank
    direct_bank_data = bank_data[bank_data['direct'] == 1].copy()
    nondirect_bank_data = bank_data[bank_data['direct'] == 0].copy()
    
    if len(direct_bank_data) == 0 or len(nondirect_bank_data) == 0:
        print(f"Warning: Insufficient data. Direct bank: {len(direct_bank_data)}, Non-direct bank: {len(nondirect_bank_data)}")
        return
    
    # Set style
    plt.style.use('default')
    sns.set_palette("husl")
    
    # Create figure with subplots (4 rows x 2 cols to include all loan characteristics)
    fig, axes = plt.subplots(4, 2, figsize=(15, 22))
    fig.suptitle('Information Covenant Usage & Loan Characteristics: Direct Bank vs Non-Direct Bank Loans', fontsize=16, y=0.995)
    
    # 1. Covenant usage by direct vs non-direct bank
    covenant_types = ['monthly_fs', 'projected_fs', 'lender_meeting']
    covenant_names = ['Monthly FS', 'Projected FS', 'Lender Meeting']
    
    direct_bank_rates = []
    nondirect_bank_rates = []
    
    for covenant in covenant_types:
        direct_bank_rate = direct_bank_data[covenant].mean()
        nondirect_bank_rate = nondirect_bank_data[covenant].mean()
        direct_bank_rates.append(direct_bank_rate)
        nondirect_bank_rates.append(nondirect_bank_rate)
    
    x = np.arange(len(covenant_names))
    width = 0.35
    
    axes[0, 0].bar(x - width/2, direct_bank_rates, width, label='Direct Bank', alpha=0.8, color='green')
    axes[0, 0].bar(x + width/2, nondirect_bank_rates, width, label='Non-Direct Bank', alpha=0.8, color='orange')
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage: Direct Bank vs Non-Direct Bank')
    axes[0, 0].set_xticks(x)
    axes[0, 0].set_xticklabels(covenant_names, rotation=45)
    axes[0, 0].legend()
    axes[0, 0].grid(True, alpha=0.3)
    
    # 2. Covenant intensity distribution
    direct_bank_intensity = direct_bank_data['total_info_covenants'].value_counts().sort_index()
    nondirect_bank_intensity = nondirect_bank_data['total_info_covenants'].value_counts().sort_index()
    
    x_intensity = np.arange(4)
    direct_bank_counts = [direct_bank_intensity.get(i, 0) for i in range(4)]
    nondirect_bank_counts = [nondirect_bank_intensity.get(i, 0) for i in range(4)]
    
    # Normalize to percentages
    direct_bank_total = sum(direct_bank_counts)
    nondirect_bank_total = sum(nondirect_bank_counts)
    direct_bank_pct = [count/direct_bank_total*100 if direct_bank_total > 0 else 0 for count in direct_bank_counts]
    nondirect_bank_pct = [count/nondirect_bank_total*100 if nondirect_bank_total > 0 else 0 for count in nondirect_bank_counts]
    
    axes[0, 1].bar(x_intensity - width/2, direct_bank_pct, width, label='Direct Bank', alpha=0.8, color='green')
    axes[0, 1].bar(x_intensity + width/2, nondirect_bank_pct, width, label='Non-Direct Bank', alpha=0.8, color='orange')
    axes[0, 1].set_xlabel('Number of Covenant Types')
    axes[0, 1].set_ylabel('Percentage of Loans')
    axes[0, 1].set_title('Covenant Intensity Distribution')
    axes[0, 1].set_xticks(x_intensity)
    axes[0, 1].set_xticklabels(['0', '1', '2', '3'])
    axes[0, 1].legend()
    axes[0, 1].grid(True, alpha=0.3)
    
    # 3. Summary statistics table
    summary_stats = {
        'Direct Bank': {
            'Count': len(direct_bank_data),
            'Avg Covenants': direct_bank_data['total_info_covenants'].mean(),
            'Monthly FS %': direct_bank_data['monthly_fs'].mean() * 100,
            'Projected FS %': direct_bank_data['projected_fs'].mean() * 100,
            'Meeting %': direct_bank_data['lender_meeting'].mean() * 100
        },
        'Non-Direct Bank': {
            'Count': len(nondirect_bank_data),
            'Avg Covenants': nondirect_bank_data['total_info_covenants'].mean(),
            'Monthly FS %': nondirect_bank_data['monthly_fs'].mean() * 100,
            'Projected FS %': nondirect_bank_data['projected_fs'].mean() * 100,
            'Meeting %': nondirect_bank_data['lender_meeting'].mean() * 100
        }
    }
    
    # Create a text summary
    summary_text = "Summary Statistics:\n\n"
    summary_text += "Direct Bank Loans:\n"
    summary_text += f"  Count: {summary_stats['Direct Bank']['Count']:,}\n"
    summary_text += f"  Avg Covenants: {summary_stats['Direct Bank']['Avg Covenants']:.2f}\n"
    summary_text += f"  Monthly FS: {summary_stats['Direct Bank']['Monthly FS %']:.1f}%\n"
    summary_text += f"  Projected FS: {summary_stats['Direct Bank']['Projected FS %']:.1f}%\n"
    summary_text += f"  Meeting: {summary_stats['Direct Bank']['Meeting %']:.1f}%\n\n"
    summary_text += "Non-Direct Bank Loans:\n"
    summary_text += f"  Count: {summary_stats['Non-Direct Bank']['Count']:,}\n"
    summary_text += f"  Avg Covenants: {summary_stats['Non-Direct Bank']['Avg Covenants']:.2f}\n"
    summary_text += f"  Monthly FS: {summary_stats['Non-Direct Bank']['Monthly FS %']:.1f}%\n"
    summary_text += f"  Projected FS: {summary_stats['Non-Direct Bank']['Projected FS %']:.1f}%\n"
    summary_text += f"  Meeting: {summary_stats['Non-Direct Bank']['Meeting %']:.1f}%"
    
    axes[1, 0].text(0.1, 0.5, summary_text, transform=axes[1, 0].transAxes,
                    fontsize=11, verticalalignment='center', family='monospace',
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    axes[1, 0].set_xlim(0, 1)
    axes[1, 0].set_ylim(0, 1)
    axes[1, 0].axis('off')
    axes[1, 0].set_title('Summary Statistics')
    
    # 4. Time trend of covenant usage
    direct_bank_yearly = direct_bank_data.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
    nondirect_bank_yearly = nondirect_bank_data.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
    
    # Filter years with sufficient data
    direct_bank_yearly = direct_bank_yearly[direct_bank_yearly['count'] >= 5]
    nondirect_bank_yearly = nondirect_bank_yearly[nondirect_bank_yearly['count'] >= 10]
    
    if len(direct_bank_yearly) > 0 and len(nondirect_bank_yearly) > 0:
        ax_twin = axes[1, 1].twinx()
        line1 = axes[1, 1].plot(direct_bank_yearly['year'], direct_bank_yearly['mean'], 'g-o', label='Direct Bank Avg', linewidth=2)
        line2 = axes[1, 1].plot(nondirect_bank_yearly['year'], nondirect_bank_yearly['mean'], 'orange', marker='s', linestyle='-', label='Non-Direct Bank Avg', linewidth=2)
        line3 = ax_twin.plot(direct_bank_yearly['year'], direct_bank_yearly['count'], 'g--', alpha=0.5, label='Direct Bank Count')
        line4 = ax_twin.plot(nondirect_bank_yearly['year'], nondirect_bank_yearly['count'], 'orange', linestyle='--', alpha=0.5, label='Non-Direct Bank Count')
        
        axes[1, 1].set_xlabel('Year')
        axes[1, 1].set_ylabel('Average Covenants', color='black')
        ax_twin.set_ylabel('Number of Loans', color='gray')
        axes[1, 1].set_title('Covenant Usage Trend Over Time')
        axes[1, 1].grid(True, alpha=0.3)
        
        # Combine legends
        lines = line1 + line2 + line3 + line4
        labels = [l.get_label() for l in lines]
        axes[1, 1].legend(lines, labels, loc='upper left', fontsize=8)
    else:
        axes[1, 1].text(0.5, 0.5, 'Insufficient data for time trend', 
                       transform=axes[1, 1].transAxes, ha='center', va='center')
        axes[1, 1].set_title('Covenant Usage Trend Over Time')
    
    # Initialize variables for loan characteristics
    direct_bank_amount = pd.Series([])
    nondirect_bank_amount = pd.Series([])
    direct_bank_spread = pd.Series([])
    nondirect_bank_spread = pd.Series([])
    direct_bank_maturity = pd.Series([])
    nondirect_bank_maturity = pd.Series([])
    
    # 5. Deal Amount (Facility Amount) Comparison
    if 'facility_amount' in direct_bank_data.columns and 'facility_amount' in nondirect_bank_data.columns:
        direct_bank_amount = pd.to_numeric(direct_bank_data['facility_amount'], errors='coerce').dropna()
        nondirect_bank_amount = pd.to_numeric(nondirect_bank_data['facility_amount'], errors='coerce').dropna()
        
        if len(direct_bank_amount) > 0 and len(nondirect_bank_amount) > 0:
            # Box plot comparison
            data_to_plot = [direct_bank_amount, nondirect_bank_amount]
            bp = axes[2, 0].boxplot(data_to_plot, labels=['Direct Bank', 'Non-Direct Bank'], patch_artist=True)
            bp['boxes'][0].set_facecolor('green')
            bp['boxes'][0].set_alpha(0.7)
            bp['boxes'][1].set_facecolor('orange')
            bp['boxes'][1].set_alpha(0.7)
            
            axes[2, 0].set_ylabel('Facility Amount (Millions USD)')
            axes[2, 0].set_title('Deal Amount Distribution')
            axes[2, 0].grid(True, alpha=0.3, axis='y')
            
            # Add mean values as text
            mean_direct = direct_bank_amount.mean()
            mean_nondirect = nondirect_bank_amount.mean()
            axes[2, 0].text(0.5, 0.95, f'Mean Direct: ${mean_direct:.1f}M\nMean Non-Direct: ${mean_nondirect:.1f}M',
                           transform=axes[2, 0].transAxes, ha='center', va='top',
                           bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5), fontsize=9)
        else:
            axes[2, 0].text(0.5, 0.5, 'Insufficient data for deal amount', 
                           transform=axes[2, 0].transAxes, ha='center', va='center')
            axes[2, 0].set_title('Deal Amount Distribution')
    else:
        axes[2, 0].text(0.5, 0.5, 'Facility amount data not available', 
                       transform=axes[2, 0].transAxes, ha='center', va='center')
        axes[2, 0].set_title('Deal Amount Distribution')
    
    # 6. Interest Spread Comparison
    if 'clean_interest_spread' in direct_bank_data.columns and 'clean_interest_spread' in nondirect_bank_data.columns:
        direct_bank_spread = pd.to_numeric(direct_bank_data['clean_interest_spread'], errors='coerce').dropna()
        nondirect_bank_spread = pd.to_numeric(nondirect_bank_data['clean_interest_spread'], errors='coerce').dropna()
        
        if len(direct_bank_spread) > 0 and len(nondirect_bank_spread) > 0:
            # Box plot comparison
            data_to_plot = [direct_bank_spread, nondirect_bank_spread]
            bp = axes[2, 1].boxplot(data_to_plot, labels=['Direct Bank', 'Non-Direct Bank'], patch_artist=True)
            bp['boxes'][0].set_facecolor('green')
            bp['boxes'][0].set_alpha(0.7)
            bp['boxes'][1].set_facecolor('orange')
            bp['boxes'][1].set_alpha(0.7)
            
            axes[2, 1].set_ylabel('Interest Spread (basis points)')
            axes[2, 1].set_title('Interest Spread Distribution')
            axes[2, 1].grid(True, alpha=0.3, axis='y')
            
            # Add mean values as text
            mean_direct = direct_bank_spread.mean()
            mean_nondirect = nondirect_bank_spread.mean()
            axes[2, 1].text(0.5, 0.95, f'Mean Direct: {mean_direct:.1f} bps\nMean Non-Direct: {mean_nondirect:.1f} bps',
                           transform=axes[2, 1].transAxes, ha='center', va='top',
                           bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5), fontsize=9)
        else:
            axes[2, 1].text(0.5, 0.5, 'Insufficient data for interest spread', 
                           transform=axes[2, 1].transAxes, ha='center', va='center')
            axes[2, 1].set_title('Interest Spread Distribution')
    else:
        axes[2, 1].text(0.5, 0.5, 'Interest spread data not available', 
                       transform=axes[2, 1].transAxes, ha='center', va='center')
        axes[2, 1].set_title('Interest Spread Distribution')
    
    # 7. Maturity Comparison
    if 'maturity_months' in direct_bank_data.columns and 'maturity_months' in nondirect_bank_data.columns:
        direct_bank_maturity = pd.to_numeric(direct_bank_data['maturity_months'], errors='coerce').dropna()
        nondirect_bank_maturity = pd.to_numeric(nondirect_bank_data['maturity_months'], errors='coerce').dropna()
        
        if len(direct_bank_maturity) > 0 and len(nondirect_bank_maturity) > 0:
            # Box plot comparison
            data_to_plot = [direct_bank_maturity, nondirect_bank_maturity]
            bp = axes[3, 0].boxplot(data_to_plot, labels=['Direct Bank', 'Non-Direct Bank'], patch_artist=True)
            bp['boxes'][0].set_facecolor('green')
            bp['boxes'][0].set_alpha(0.7)
            bp['boxes'][1].set_facecolor('orange')
            bp['boxes'][1].set_alpha(0.7)
            
            axes[3, 0].set_ylabel('Maturity (months)')
            axes[3, 0].set_title('Maturity Distribution')
            axes[3, 0].grid(True, alpha=0.3, axis='y')
            
            # Add mean values as text
            mean_direct = direct_bank_maturity.mean()
            mean_nondirect = nondirect_bank_maturity.mean()
            axes[3, 0].text(0.5, 0.95, f'Mean Direct: {mean_direct:.1f} months\nMean Non-Direct: {mean_nondirect:.1f} months',
                           transform=axes[3, 0].transAxes, ha='center', va='top',
                           bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5), fontsize=9)
        else:
            axes[3, 0].text(0.5, 0.5, 'Insufficient data for maturity', 
                           transform=axes[3, 0].transAxes, ha='center', va='center')
            axes[3, 0].set_title('Maturity Distribution')
    else:
        axes[3, 0].text(0.5, 0.5, 'Maturity data not available', 
                       transform=axes[3, 0].transAxes, ha='center', va='center')
        axes[3, 0].set_title('Maturity Distribution')
    
    # 8. Combined Loan Characteristics Summary (bar chart)
    loan_chars = ['Facility Amount\n($M)', 'Interest Spread\n(bps)', 'Maturity\n(months)']
    direct_means = []
    nondirect_means = []
    
    # Use already computed values
    if len(direct_bank_amount) > 0:
        direct_means.append(direct_bank_amount.mean())
    else:
        direct_means.append(0)
    
    if len(direct_bank_spread) > 0:
        direct_means.append(direct_bank_spread.mean())
    else:
        direct_means.append(0)
    
    if len(direct_bank_maturity) > 0:
        direct_means.append(direct_bank_maturity.mean())
    else:
        direct_means.append(0)
    
    if len(nondirect_bank_amount) > 0:
        nondirect_means.append(nondirect_bank_amount.mean())
    else:
        nondirect_means.append(0)
    
    if len(nondirect_bank_spread) > 0:
        nondirect_means.append(nondirect_bank_spread.mean())
    else:
        nondirect_means.append(0)
    
    if len(nondirect_bank_maturity) > 0:
        nondirect_means.append(nondirect_bank_maturity.mean())
    else:
        nondirect_means.append(0)
    
    # Normalize for comparison (use relative values)
    x_loan_chars = np.arange(len(loan_chars))
    
    # Create normalized comparison (divide by max to show relative differences)
    max_vals = [max(abs(d), abs(n)) if (d != 0 or n != 0) else 1 for d, n in zip(direct_means, nondirect_means)]
    direct_normalized = [d/m if m > 0 else 0 for d, m in zip(direct_means, max_vals)]
    nondirect_normalized = [n/m if m > 0 else 0 for n, m in zip(nondirect_means, max_vals)]
    
    # Or just show actual values with different scales - let's use a grouped bar chart
    axes[3, 1].bar(x_loan_chars - width/2, direct_means, width, label='Direct Bank', alpha=0.8, color='green')
    axes[3, 1].bar(x_loan_chars + width/2, nondirect_means, width, label='Non-Direct Bank', alpha=0.8, color='orange')
    axes[3, 1].set_xlabel('Loan Characteristic')
    axes[3, 1].set_ylabel('Mean Value')
    axes[3, 1].set_title('Loan Characteristics Comparison (Mean Values)')
    axes[3, 1].set_xticks(x_loan_chars)
    axes[3, 1].set_xticklabels(loan_chars)
    axes[3, 1].legend()
    axes[3, 1].grid(True, alpha=0.3, axis='y')
    
    # Add maturity comparison as a bar chart in the summary stats area or create a combined plot
    # Let's update the summary stats to include these metrics
    if 'maturity_months' in direct_bank_data.columns and 'maturity_months' in nondirect_bank_data.columns:
        direct_bank_maturity = pd.to_numeric(direct_bank_data['maturity_months'], errors='coerce').dropna()
        nondirect_bank_maturity = pd.to_numeric(nondirect_bank_data['maturity_months'], errors='coerce').dropna()
        
        # Update summary stats text to include loan characteristics
        summary_stats['Direct Bank']['Avg Facility Amount'] = direct_bank_amount.mean() if len(direct_bank_amount) > 0 else np.nan
        summary_stats['Direct Bank']['Avg Interest Spread'] = direct_bank_spread.mean() if len(direct_bank_spread) > 0 else np.nan
        summary_stats['Direct Bank']['Avg Maturity'] = direct_bank_maturity.mean() if len(direct_bank_maturity) > 0 else np.nan
        
        summary_stats['Non-Direct Bank']['Avg Facility Amount'] = nondirect_bank_amount.mean() if len(nondirect_bank_amount) > 0 else np.nan
        summary_stats['Non-Direct Bank']['Avg Interest Spread'] = nondirect_bank_spread.mean() if len(nondirect_bank_spread) > 0 else np.nan
        summary_stats['Non-Direct Bank']['Avg Maturity'] = nondirect_bank_maturity.mean() if len(nondirect_bank_maturity) > 0 else np.nan
        
        # Update summary text
        summary_text = "Summary Statistics:\n\n"
        summary_text += "Direct Bank Loans:\n"
        summary_text += f"  Count: {summary_stats['Direct Bank']['Count']:,}\n"
        summary_text += f"  Avg Covenants: {summary_stats['Direct Bank']['Avg Covenants']:.2f}\n"
        summary_text += f"  Monthly FS: {summary_stats['Direct Bank']['Monthly FS %']:.1f}%\n"
        summary_text += f"  Projected FS: {summary_stats['Direct Bank']['Projected FS %']:.1f}%\n"
        summary_text += f"  Meeting: {summary_stats['Direct Bank']['Meeting %']:.1f}%\n"
        if not np.isnan(summary_stats['Direct Bank']['Avg Facility Amount']):
            summary_text += f"  Avg Facility: ${summary_stats['Direct Bank']['Avg Facility Amount']:.1f}M\n"
        if not np.isnan(summary_stats['Direct Bank']['Avg Interest Spread']):
            summary_text += f"  Avg Spread: {summary_stats['Direct Bank']['Avg Interest Spread']:.1f} bps\n"
        if not np.isnan(summary_stats['Direct Bank']['Avg Maturity']):
            summary_text += f"  Avg Maturity: {summary_stats['Direct Bank']['Avg Maturity']:.1f} months\n"
        summary_text += "\nNon-Direct Bank Loans:\n"
        summary_text += f"  Count: {summary_stats['Non-Direct Bank']['Count']:,}\n"
        summary_text += f"  Avg Covenants: {summary_stats['Non-Direct Bank']['Avg Covenants']:.2f}\n"
        summary_text += f"  Monthly FS: {summary_stats['Non-Direct Bank']['Monthly FS %']:.1f}%\n"
        summary_text += f"  Projected FS: {summary_stats['Non-Direct Bank']['Projected FS %']:.1f}%\n"
        summary_text += f"  Meeting: {summary_stats['Non-Direct Bank']['Meeting %']:.1f}%\n"
        if not np.isnan(summary_stats['Non-Direct Bank']['Avg Facility Amount']):
            summary_text += f"  Avg Facility: ${summary_stats['Non-Direct Bank']['Avg Facility Amount']:.1f}M\n"
        if not np.isnan(summary_stats['Non-Direct Bank']['Avg Interest Spread']):
            summary_text += f"  Avg Spread: {summary_stats['Non-Direct Bank']['Avg Interest Spread']:.1f} bps\n"
        if not np.isnan(summary_stats['Non-Direct Bank']['Avg Maturity']):
            summary_text += f"  Avg Maturity: {summary_stats['Non-Direct Bank']['Avg Maturity']:.1f} months"
        
        # Update the summary stats text box (make it more compact)
        axes[1, 0].clear()
        axes[1, 0].text(0.05, 0.5, summary_text, transform=axes[1, 0].transAxes,
                        fontsize=9, verticalalignment='center', family='monospace',
                        bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
        axes[1, 0].set_xlim(0, 1)
        axes[1, 0].set_ylim(0, 1)
        axes[1, 0].axis('off')
        axes[1, 0].set_title('Summary Statistics')
        
        # Add maturity bar chart comparison
        if len(direct_bank_maturity) > 0 and len(nondirect_bank_maturity) > 0:
            # Create a bar chart for maturity comparison
            maturity_means = [direct_bank_maturity.mean(), nondirect_bank_maturity.mean()]
            maturity_stds = [direct_bank_maturity.std(), nondirect_bank_maturity.std()]
            x_maturity = np.arange(2)
            
            # Add as inset or modify time trend to show maturity
            # Actually, let's add it as text annotation on the time trend plot or create a small subplot
            # For now, add maturity info to the summary stats area as additional text
            pass
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6c_InfoCovenants_Detailed_DirectBank_vs_NonDirectBank.png", dpi=300, bbox_inches='tight')
    plt.close()




def main():
    print("=" * 80)
    print("6c_DescribeInfoCov.py - Analyze Information Covenant Time Series")
    print("=" * 80)

    # Set up paths
    script_dir = Path(__file__).parent
    input_file = script_dir / ".." / "Data" / "Intermediate" / "6b_PanelWithInfoCovenants.csv"
    fig_dir = ensure_fig_dir()

    # Check if input file exists
    if not input_file.exists():
        print(f"Error: Input file not found: {input_file}")
        print("Please run 6b_AggregateInfoCov.py first to generate the panel with covenants.")
        return

    print(f"Input file: {input_file}")
    print(f"Output directory: {fig_dir}")

    # Load data
    print("Loading panel data with information covenants...")
    df = pd.read_csv(input_file)
    print(f"Loaded {len(df)} observations")

    # Extract year from deal_active_date
    df['year'] = extract_year_from_date('deal_active_date', df)
    
    # Filter to observations with valid years (keep all years)
    df = df[df['year'].notna()]
    print(f"After filtering by year: {len(df)} observations")
    
    print(f"Year range: {df['year'].min()} to {df['year'].max()}")

    # Create visualizations
    print("\nCreating time series plots...")
    
    create_time_series_direct_vs_nondirect(df, fig_dir)
    print("  - Direct vs Non-Direct time series plot created")
    
    create_detailed_comparison_direct_bank_vs_nondirect_bank(df, fig_dir)
    print("  - Detailed comparison plots (Direct Bank vs Non-Direct Bank) created")

    print(f"\nAll figures saved to: {fig_dir}")
    print("Done.")


if __name__ == "__main__":
    main()
