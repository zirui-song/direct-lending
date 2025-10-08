#!/usr/bin/env python3
"""
6c_DescribeInfoCov.py

Analyze and visualize time series of information covenant usage by lender type.

Creates time series plots showing:
1. Information covenant usage over time by bank vs nonbank lenders
2. Information covenant usage over time by nonbank lender types
3. Detailed breakdown of covenant types by lender category

Inputs:
- ../Data/Intermediate/6b_PanelWithInfoCovenants.csv

Outputs:
- ../Results/Figures/6c_InfoCovenants_TimeSeries_Bank_vs_Nonbank.png
- ../Results/Figures/6c_InfoCovenants_TimeSeries_NonbankTypes.png
- ../Results/Figures/6c_InfoCovenants_Detailed_Bank_vs_Nonbank.png
- ../Results/Figures/6c_InfoCovenants_Detailed_NonbankTypes.png

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


def create_time_series_bank_vs_nonbank(df, fig_dir):
    """Create time series plots comparing bank vs nonbank covenant usage."""
    
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
    
    # Filter years with sufficient data (at least 10 observations)
    bank_yearly = bank_yearly[bank_yearly['count'] >= 10]
    nonbank_yearly = nonbank_yearly[nonbank_yearly['count'] >= 10]
    
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
                  name='Bank', line=dict(color='blue'), mode='lines+markers'),
        row=1, col=1
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['monthly_fs_rate'],
                  name='Nonbank', line=dict(color='red'), mode='lines+markers'),
        row=1, col=1
    )
    
    # Plot 2: Projected FS
    fig.add_trace(
        go.Scatter(x=bank_yearly['year'], y=bank_yearly['projected_fs_rate'],
                  name='Bank', line=dict(color='blue'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['projected_fs_rate'],
                  name='Nonbank', line=dict(color='red'), mode='lines+markers', showlegend=False),
        row=1, col=2
    )
    
    # Plot 3: Lender Meetings
    fig.add_trace(
        go.Scatter(x=bank_yearly['year'], y=bank_yearly['meeting_rate'],
                  name='Bank', line=dict(color='blue'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['meeting_rate'],
                  name='Nonbank', line=dict(color='red'), mode='lines+markers', showlegend=False),
        row=2, col=1
    )
    
    # Plot 4: Average Total Covenants
    fig.add_trace(
        go.Scatter(x=bank_yearly['year'], y=bank_yearly['avg_covenants'],
                  name='Bank', line=dict(color='blue'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    fig.add_trace(
        go.Scatter(x=nonbank_yearly['year'], y=nonbank_yearly['avg_covenants'],
                  name='Nonbank', line=dict(color='red'), mode='lines+markers', showlegend=False),
        row=2, col=2
    )
    
    fig.update_layout(
        title='Information Covenant Usage Over Time: Bank vs Nonbank Lenders',
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
    pio.write_image(fig, str(fig_dir / "6c_InfoCovenants_TimeSeries_Bank_vs_Nonbank.png"), 
                   width=1200, height=800, scale=2)


def create_time_series_nonbank_types(df, fig_dir):
    """Create time series plots for different nonbank lender types."""
    
    # Get nonbank data only
    nonbank_data = df[df['nonbank_lender'] == 1].copy()
    
    if 'lender_type' not in nonbank_data.columns:
        print("Warning: lender_type column not found. Cannot create nonbank types analysis.")
        return
    
    # Get top nonbank lender types by count
    lender_counts = nonbank_data['lender_type'].value_counts()
    top_lender_types = lender_counts.head(6).index.tolist()  # Top 6 for readability
    
    print(f"Top nonbank lender types: {top_lender_types}")
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
        
        # Filter years with sufficient data
        yearly_data = yearly_data[yearly_data['count'] >= 5]
        
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
        title='Information Covenant Usage Over Time: Nonbank Lender Types',
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
    pio.write_image(fig, str(fig_dir / "6c_InfoCovenants_TimeSeries_NonbankTypes.png"),
                   width=1200, height=800, scale=2)


def create_detailed_comparison_plots(df, fig_dir):
    """Create detailed comparison plots using matplotlib/seaborn."""
    
    # Set style
    plt.style.use('default')
    sns.set_palette("husl")
    
    # Create figure with subplots
    fig, axes = plt.subplots(2, 2, figsize=(15, 12))
    fig.suptitle('Information Covenant Usage: Detailed Analysis', fontsize=16, y=0.98)
    
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
    
    axes[0, 0].bar(x - width/2, bank_rates, width, label='Bank', alpha=0.8)
    axes[0, 0].bar(x + width/2, nonbank_rates, width, label='Nonbank', alpha=0.8)
    axes[0, 0].set_xlabel('Covenant Type')
    axes[0, 0].set_ylabel('Usage Rate')
    axes[0, 0].set_title('Covenant Usage: Bank vs Nonbank')
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
    
    axes[0, 1].bar(x_intensity - width/2, bank_pct, width, label='Bank', alpha=0.8)
    axes[0, 1].bar(x_intensity + width/2, nonbank_pct, width, label='Nonbank', alpha=0.8)
    axes[0, 1].set_xlabel('Number of Covenant Types')
    axes[0, 1].set_ylabel('Percentage of Loans')
    axes[0, 1].set_title('Covenant Intensity Distribution')
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
        axes[1, 0].set_title('Average Covenants by Nonbank Lender Type')
        axes[1, 0].grid(True, alpha=0.3)
    
    # 4. Time trend of overall covenant usage
    yearly_trend = df.groupby('year')['total_info_covenants'].agg(['mean', 'count']).reset_index()
    yearly_trend = yearly_trend[yearly_trend['count'] >= 10]  # Filter years with sufficient data
    
    ax_twin = axes[1, 1].twinx()
    line1 = axes[1, 1].plot(yearly_trend['year'], yearly_trend['mean'], 'b-o', label='Avg Covenants')
    line2 = ax_twin.plot(yearly_trend['year'], yearly_trend['count'], 'r-s', label='Loan Count')
    
    axes[1, 1].set_xlabel('Year')
    axes[1, 1].set_ylabel('Average Covenants', color='b')
    ax_twin.set_ylabel('Number of Loans', color='r')
    axes[1, 1].set_title('Covenant Usage Trend Over Time')
    axes[1, 1].grid(True, alpha=0.3)
    
    # Combine legends
    lines = line1 + line2
    labels = [l.get_label() for l in lines]
    axes[1, 1].legend(lines, labels, loc='upper left')
    
    plt.tight_layout()
    plt.savefig(fig_dir / "6c_InfoCovenants_Detailed_Analysis.png", dpi=300, bbox_inches='tight')
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
    
    # Filter to observations with valid years
    df = df[df['year'].notna() & (df['year'] >= 2010) & (df['year'] <= 2023)]
    print(f"After filtering by year: {len(df)} observations")
    
    print(f"Year range: {df['year'].min()} to {df['year'].max()}")
    print(f"Bank loans: {len(df[df['nonbank_lender'] == 0])}")
    print(f"Nonbank loans: {len(df[df['nonbank_lender'] == 1])}")

    # Create visualizations
    print("\nCreating time series plots...")
    create_time_series_bank_vs_nonbank(df, fig_dir)
    print("  - Bank vs Nonbank time series plot created")
    
    create_time_series_nonbank_types(df, fig_dir)
    print("  - Nonbank types time series plot created")
    
    create_detailed_comparison_plots(df, fig_dir)
    print("  - Detailed comparison plots created")

    print(f"\nAll figures saved to: {fig_dir}")
    print("Done.")


if __name__ == "__main__":
    main()
