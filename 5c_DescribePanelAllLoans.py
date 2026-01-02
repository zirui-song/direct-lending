#!/usr/bin/env python3
"""
5c_DescribePanelAllLoans.py

Generates descriptive statistics and visualizations for the panel of all loans,
focusing on direct vs non-direct loan comparisons.

Inputs:
- ../Data/Intermediate/5c_PanelAllLoans.csv

Outputs:
- Time series plots of number of loans (direct vs non-direct)
- Time series plots of average facility amount, interest spread, and maturity
- Comparison plots for direct vs non-direct loans

Author: Zirui Song
Date: Dec 2025
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
import plotly.io as pio
from datetime import datetime
import re


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
        year_match = re.search(r'(19|20)\d{2}', str(date_str))
        if year_match:
            return int(year_match.group())
        return None
    
    return date_series.apply(extract_year)


def clean_data(df):
    """Clean and prepare data for analysis"""
    print("Cleaning data...")
    
    # Convert numeric columns
    numeric_cols = ['facility_amount', 'maturity_months', 'interest_spread', 
                   'clean_interest_spread']
    
    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    # Create direct indicator
    if 'direct_from_text' in df.columns:
        df['direct'] = df['direct_from_text'].fillna(0).astype(int)
    else:
        df['direct'] = 0
        print("Warning: direct_from_text column not found, setting all loans as non-direct")
    
    # Extract year from deal_active_date if year column doesn't exist
    if 'year' not in df.columns:
        if 'deal_active_date' in df.columns:
            df['year'] = extract_year_from_date('deal_active_date', df)
        elif 'fyear' in df.columns:
            df['year'] = df['fyear']
        else:
            print("Warning: No year column or date column found")
            df['year'] = None
    
    return df


def create_time_series_loan_counts(df, fig_dir):
    """Create time series plot of number of direct vs non-direct loans"""
    print("Creating time series plot of loan counts...")
    
    # Filter to observations with valid years
    df_with_year = df[df['year'].notna()].copy()
    
    if len(df_with_year) == 0:
        print("Warning: No valid years found for time series plot")
        return
    
    # Count loans by year and direct status
    yearly_counts = df_with_year.groupby(['year', 'direct']).size().unstack(fill_value=0)
    yearly_counts.columns = ['Non-Direct', 'Direct']
    
    # Create the plot
    fig = go.Figure()
    
    # Add traces for each group
    colors = {"Direct": "#2ca02c", "Non-Direct": "#d62728"}
    
    for group in ["Direct", "Non-Direct"]:
        if group in yearly_counts.columns:
            fig.add_trace(go.Scatter(
                x=yearly_counts.index,
                y=yearly_counts[group],
                mode='lines+markers',
                name=group,
                line=dict(color=colors[group], width=3),
                marker=dict(size=8)
            ))
    
    # Update layout
    fig.update_layout(
        title='Number of Loans Over Time: Direct vs Non-Direct',
        xaxis_title='Year',
        yaxis_title='Number of Loans',
        title_x=0.5,
        height=500,
        width=1000,
        legend=dict(
            orientation="h",
            yanchor="bottom",
            y=1.02,
            xanchor="right",
            x=1
        )
    )
    
    # Add grid
    fig.update_xaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
    fig.update_yaxes(showgrid=True, gridwidth=1, gridcolor='lightgray')
    
    # Save the plot
    pio.write_image(fig, str(fig_dir / "5c_TimeSeries_LoanCounts_Direct_vs_NonDirect.png"), 
                   width=1000, height=500, scale=2)
    print(f"Time series plot created: {len(df_with_year)} observations from {df_with_year['year'].min()} to {df_with_year['year'].max()}")


def create_time_series_averages(df, fig_dir):
    """Create time series plots of average facility amount, interest spread, and maturity"""
    print("Creating time series plots of average loan characteristics...")
    
    # Filter to observations with valid years
    df_with_year = df[df['year'].notna()].copy()
    
    if len(df_with_year) == 0:
        print("Warning: No valid years found for time series plot")
        return
    
    # Variables to plot
    variables = {
        'facility_amount': {'title': 'Average Facility Amount (Millions USD)', 'unit': 'M'},
        'clean_interest_spread': {'title': 'Average Interest Spread (bps)', 'unit': 'bps'},
        'maturity_months': {'title': 'Average Maturity (Months)', 'unit': 'months'}
    }
    
    # Create subplots
    fig = make_subplots(
        rows=1, cols=3,
        subplot_titles=[var_info['title'] for var, var_info in variables.items()],
        vertical_spacing=0.15
    )
    
    colors = {"Direct": "#2ca02c", "Non-Direct": "#d62728"}
    
    for i, (var, var_info) in enumerate(variables.items(), 1):
        if var not in df_with_year.columns:
            print(f"Warning: {var} not found in dataset")
            continue
        
        # Calculate yearly averages by direct status
        yearly_avg = df_with_year.groupby(['year', 'direct'])[var].mean().unstack(fill_value=0)
        yearly_avg.columns = ['Non-Direct', 'Direct']
        
        # Filter years with at least 5 observations per group
        yearly_counts = df_with_year.groupby(['year', 'direct']).size().unstack(fill_value=0)
        yearly_counts.columns = ['Non-Direct', 'Direct']
        
        # Only plot years with sufficient data
        valid_years = yearly_counts[(yearly_counts['Direct'] >= 5) & (yearly_counts['Non-Direct'] >= 5)].index
        yearly_avg_filtered = yearly_avg.loc[valid_years]
        
        for group in ["Direct", "Non-Direct"]:
            if group in yearly_avg_filtered.columns:
                fig.add_trace(
                    go.Scatter(
                        x=yearly_avg_filtered.index,
                        y=yearly_avg_filtered[group],
                        mode='lines+markers',
                        name=group,
                        line=dict(color=colors[group], width=3),
                        marker=dict(size=8),
                        showlegend=(i == 1),  # Only show legend for first subplot
                        hovertemplate=f'<b>{group}</b><br>' +
                                     f'Year: %{{x}}<br>' +
                                     f'{var_info["title"]}: %{{y:.2f}}<br>' +
                                     '<extra></extra>'
                    ),
                    row=1, col=i
                )
        
        # Update y-axis labels
        fig.update_yaxes(title_text=var_info['title'], row=1, col=i)
    
    # Update layout
    fig.update_layout(
        title='Average Loan Characteristics Over Time: Direct vs Non-Direct',
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
    
    # Update x-axis labels
    fig.update_xaxes(title_text="Year", row=1, col=2)
    
    # Add grid
    for i in range(1, 4):
        fig.update_xaxes(showgrid=True, gridwidth=1, gridcolor='lightgray', row=1, col=i)
        fig.update_yaxes(showgrid=True, gridwidth=1, gridcolor='lightgray', row=1, col=i)
    
    # Save the plot
    pio.write_image(fig, str(fig_dir / "5c_TimeSeries_AverageCharacteristics_Direct_vs_NonDirect.png"), 
                   width=1500, height=500, scale=2)
    print("Time series plots of average characteristics created")


def create_comparison_plots(df, fig_dir):
    """Create comparison plots for direct vs non-direct loans"""
    print("Creating comparison plots...")
    
    # Set style
    plt.style.use('default')
    sns.set_palette("husl")
    
    # Create figure with subplots
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('Direct vs Non-Direct Loans: Comparison', fontsize=16, y=0.98)
    
    # 1. Facility Amount comparison
    direct_amount = pd.to_numeric(df[df['direct'] == 1]['facility_amount'], errors='coerce').dropna()
    nondirect_amount = pd.to_numeric(df[df['direct'] == 0]['facility_amount'], errors='coerce').dropna()
    
    if len(direct_amount) > 0 and len(nondirect_amount) > 0:
        # Remove outliers for better visualization
        q99_direct = direct_amount.quantile(0.99)
        q99_nondirect = nondirect_amount.quantile(0.99)
        direct_amount_clean = direct_amount[direct_amount <= q99_direct]
        nondirect_amount_clean = nondirect_amount[nondirect_amount <= q99_nondirect]
        
        data_to_plot = [direct_amount_clean, nondirect_amount_clean]
        bp = axes[0, 0].boxplot(data_to_plot, tick_labels=['Direct', 'Non-Direct'], patch_artist=True)
        bp['boxes'][0].set_facecolor('#2ca02c')
        bp['boxes'][0].set_alpha(0.7)
        bp['boxes'][1].set_facecolor('#d62728')
        bp['boxes'][1].set_alpha(0.7)
        
        axes[0, 0].set_ylabel('Facility Amount (Millions USD)')
        axes[0, 0].set_title('Facility Amount Distribution')
        axes[0, 0].grid(True, alpha=0.3, axis='y')
        
        mean_direct = direct_amount.mean()
        mean_nondirect = nondirect_amount.mean()
        median_direct = direct_amount.median()
        median_nondirect = nondirect_amount.median()
        
        axes[0, 0].text(0.5, 0.95, 
                    f'Direct: Mean=${mean_direct:.1f}M, Median=${median_direct:.1f}M (N={len(direct_amount):,})\n'
                    f'Non-Direct: Mean=${mean_nondirect:.1f}M, Median=${median_nondirect:.1f}M (N={len(nondirect_amount):,})',
                    transform=axes[0, 0].transAxes, ha='center', va='top',
                    bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5), fontsize=10)
    
    # 2. Interest Spread comparison
    spread_col = 'clean_interest_spread' if 'clean_interest_spread' in df.columns else 'interest_spread'
    if spread_col in df.columns:
        direct_spread = pd.to_numeric(df[df['direct'] == 1][spread_col], errors='coerce').dropna()
        nondirect_spread = pd.to_numeric(df[df['direct'] == 0][spread_col], errors='coerce').dropna()
        
        # Filter reasonable range
        direct_spread = direct_spread[(direct_spread >= 0) & (direct_spread <= 2000)]
        nondirect_spread = nondirect_spread[(nondirect_spread >= 0) & (nondirect_spread <= 2000)]
        
        if len(direct_spread) > 0 and len(nondirect_spread) > 0:
            data_to_plot = [direct_spread, nondirect_spread]
            bp = axes[0, 1].boxplot(data_to_plot, tick_labels=['Direct', 'Non-Direct'], patch_artist=True)
            bp['boxes'][0].set_facecolor('#2ca02c')
            bp['boxes'][0].set_alpha(0.7)
            bp['boxes'][1].set_facecolor('#d62728')
            bp['boxes'][1].set_alpha(0.7)
            
            axes[0, 1].set_ylabel('Interest Spread (basis points)')
            axes[0, 1].set_title('Interest Spread Distribution')
            axes[0, 1].grid(True, alpha=0.3, axis='y')
            
            mean_direct = direct_spread.mean()
            mean_nondirect = nondirect_spread.mean()
            median_direct = direct_spread.median()
            median_nondirect = nondirect_spread.median()
            
            axes[0, 1].text(0.5, 0.95, 
                        f'Direct: Mean={mean_direct:.1f} bps, Median={median_direct:.1f} bps (N={len(direct_spread):,})\n'
                        f'Non-Direct: Mean={mean_nondirect:.1f} bps, Median={median_nondirect:.1f} bps (N={len(nondirect_spread):,})',
                        transform=axes[0, 1].transAxes, ha='center', va='top',
                        bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5), fontsize=10)
    
    # 3. Maturity comparison
    if 'maturity_months' in df.columns:
        direct_maturity = pd.to_numeric(df[df['direct'] == 1]['maturity_months'], errors='coerce').dropna()
        nondirect_maturity = pd.to_numeric(df[df['direct'] == 0]['maturity_months'], errors='coerce').dropna()
        
        if len(direct_maturity) > 0 and len(nondirect_maturity) > 0:
            data_to_plot = [direct_maturity, nondirect_maturity]
            bp = axes[1, 0].boxplot(data_to_plot, tick_labels=['Direct', 'Non-Direct'], patch_artist=True)
            bp['boxes'][0].set_facecolor('#2ca02c')
            bp['boxes'][0].set_alpha(0.7)
            bp['boxes'][1].set_facecolor('#d62728')
            bp['boxes'][1].set_alpha(0.7)
            
            axes[1, 0].set_ylabel('Maturity (Months)')
            axes[1, 0].set_title('Maturity Distribution')
            axes[1, 0].grid(True, alpha=0.3, axis='y')
            
            mean_direct = direct_maturity.mean()
            mean_nondirect = nondirect_maturity.mean()
            median_direct = direct_maturity.median()
            median_nondirect = nondirect_maturity.median()
            
            axes[1, 0].text(0.5, 0.95, 
                        f'Direct: Mean={mean_direct:.1f} months, Median={median_direct:.1f} months (N={len(direct_maturity):,})\n'
                        f'Non-Direct: Mean={mean_nondirect:.1f} months, Median={median_nondirect:.1f} months (N={len(nondirect_maturity):,})',
                        transform=axes[1, 0].transAxes, ha='center', va='top',
                        bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5), fontsize=10)
    
    # 4. Summary statistics table
    axes[1, 1].axis('off')
    
    # Create summary statistics
    summary_text = "Summary Statistics:\n\n"
    
    total = len(df)
    direct_count = int(df['direct'].sum())
    nondirect_count = total - direct_count
    
    summary_text += f"Total Loans: {total:,}\n"
    summary_text += f"  Direct: {direct_count:,} ({direct_count/total*100:.1f}%)\n"
    summary_text += f"  Non-Direct: {nondirect_count:,} ({nondirect_count/total*100:.1f}%)\n\n"
    
    if 'facility_amount' in df.columns:
        direct_amt = pd.to_numeric(df[df['direct'] == 1]['facility_amount'], errors='coerce').dropna()
        nondirect_amt = pd.to_numeric(df[df['direct'] == 0]['facility_amount'], errors='coerce').dropna()
        if len(direct_amt) > 0 and len(nondirect_amt) > 0:
            summary_text += f"Facility Amount:\n"
            summary_text += f"  Direct: ${direct_amt.mean():.1f}M (median: ${direct_amt.median():.1f}M)\n"
            summary_text += f"  Non-Direct: ${nondirect_amt.mean():.1f}M (median: ${nondirect_amt.median():.1f}M)\n\n"
    
    spread_col = 'clean_interest_spread' if 'clean_interest_spread' in df.columns else 'interest_spread'
    if spread_col in df.columns:
        direct_spread = pd.to_numeric(df[df['direct'] == 1][spread_col], errors='coerce').dropna()
        nondirect_spread = pd.to_numeric(df[df['direct'] == 0][spread_col], errors='coerce').dropna()
        direct_spread = direct_spread[(direct_spread >= 0) & (direct_spread <= 2000)]
        nondirect_spread = nondirect_spread[(nondirect_spread >= 0) & (nondirect_spread <= 2000)]
        if len(direct_spread) > 0 and len(nondirect_spread) > 0:
            summary_text += f"Interest Spread:\n"
            summary_text += f"  Direct: {direct_spread.mean():.1f} bps (median: {direct_spread.median():.1f} bps)\n"
            summary_text += f"  Non-Direct: {nondirect_spread.mean():.1f} bps (median: {nondirect_spread.median():.1f} bps)\n\n"
    
    if 'maturity_months' in df.columns:
        direct_mat = pd.to_numeric(df[df['direct'] == 1]['maturity_months'], errors='coerce').dropna()
        nondirect_mat = pd.to_numeric(df[df['direct'] == 0]['maturity_months'], errors='coerce').dropna()
        if len(direct_mat) > 0 and len(nondirect_mat) > 0:
            summary_text += f"Maturity:\n"
            summary_text += f"  Direct: {direct_mat.mean():.1f} months (median: {direct_mat.median():.1f} months)\n"
            summary_text += f"  Non-Direct: {nondirect_mat.mean():.1f} months (median: {nondirect_mat.median():.1f} months)\n"
    
    axes[1, 1].text(0.1, 0.5, summary_text, transform=axes[1, 1].transAxes,
                   fontsize=11, verticalalignment='center', family='monospace',
                   bbox=dict(boxstyle='round', facecolor='wheat', alpha=0.5))
    axes[1, 1].set_title('Summary Statistics')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "5c_Comparison_Direct_vs_NonDirect.png", dpi=300, bbox_inches='tight')
    plt.close()
    print("Comparison plots created")


def main():
    print("=" * 80)
    print("5c_DescribePanelAllLoans.py - Descriptive Statistics for All Loans")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    input_csv = script_dir / ".." / "Data" / "Intermediate" / "5c_PanelAllLoans.csv"
    fig_dir = Path("/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending/Results/Figures")
    
    # Create figures directory if it doesn't exist
    fig_dir.mkdir(parents=True, exist_ok=True)
    
    # Check if input file exists
    if not input_csv.exists():
        print(f"Error: Input file not found: {input_csv}")
        return
    
    # Load data
    print("Loading panel dataset...")
    try:
        df = pd.read_csv(input_csv, low_memory=False)
        print(f"Loaded {len(df)} records with {len(df.columns)} columns")
    except Exception as e:
        print(f"Error loading data: {e}")
        return
    
    # Clean data
    df = clean_data(df)
    
    # Show basic info
    print(f"\nDataset Overview:")
    print(f"Total records: {len(df)}")
    print(f"Unique accessions: {df['accession'].nunique()}")
    direct_count = int(df['direct'].sum())
    print(f"Direct loans: {direct_count} ({direct_count/len(df)*100:.1f}%)")
    print(f"Non-Direct loans: {len(df) - direct_count} ({(len(df) - direct_count)/len(df)*100:.1f}%)")
    
    if 'year' in df.columns and df['year'].notna().any():
        print(f"Year range: {df['year'].min()} to {df['year'].max()}")
    
    # Create visualizations
    print("\nCreating visualizations...")
    create_time_series_loan_counts(df, fig_dir)
    print("  - Time series of loan counts created")
    
    create_time_series_averages(df, fig_dir)
    print("  - Time series of average characteristics created")
    
    create_comparison_plots(df, fig_dir)
    print("  - Comparison plots created")
    
    print(f"\n" + "=" * 80)
    print("DESCRIPTIVE ANALYSIS COMPLETE")
    print("=" * 80)
    print(f"Figures directory: {fig_dir}")
    print(f"Files generated:")
    print(f"  - 5c_TimeSeries_LoanCounts_Direct_vs_NonDirect.png")
    print(f"  - 5c_TimeSeries_AverageCharacteristics_Direct_vs_NonDirect.png")
    print(f"  - 5c_Comparison_Direct_vs_NonDirect.png")


if __name__ == "__main__":
    main()

