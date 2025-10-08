#!/usr/bin/env python3
"""
5b_DescribePanelNonbankLoans.py

Generates descriptive statistics and histograms for the panel nonbank loans dataset.

Inputs:
- ../Data/Intermediate/5a_PanelNonbankLoans.csv

Outputs:
- ../Data/Intermediate/5b_DescriptiveStats_NonbankLoans.csv
- ../Data/Intermediate/5b_Histograms_NonbankLoans.html

Author: Zirui Song
Date: Sep 2025
"""

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from pathlib import Path
import plotly.graph_objects as go
import plotly.express as px
from plotly.subplots import make_subplots
import plotly.offline as pyo
from datetime import datetime
import plotly.io as pio


def get_fama_french_12_industry(sic_code):
    """Convert SIC code to Fama-French 12 industry classification"""
    if pd.isna(sic_code) or sic_code == '' or sic_code == 0:
        return 'Unknown'
    
    sic = int(sic_code)
    
    # Fama-French 12 Industry Classification based on exact SIC code ranges
    # 1 NoDur - Consumer Nondurables
    if (100 <= sic <= 999 or 2000 <= sic <= 2399 or 2700 <= sic <= 2749 or 
        2770 <= sic <= 2799 or 3100 <= sic <= 3199 or 3940 <= sic <= 3989):
        return 'NoDur'
    
    # 2 Durbl - Consumer Durables  
    elif (2500 <= sic <= 2519 or 2590 <= sic <= 2599 or 3630 <= sic <= 3659 or 
          3710 <= sic <= 3711 or 3714 <= sic <= 3714 or 3716 <= sic <= 3716 or 
          3750 <= sic <= 3751 or 3792 <= sic <= 3792 or 3900 <= sic <= 3939 or 
          3990 <= sic <= 3999):
        return 'Durbl'
    
    # 3 Manuf - Manufacturing
    elif (2520 <= sic <= 2589 or 2600 <= sic <= 2699 or 2750 <= sic <= 2769 or 
          3000 <= sic <= 3099 or 3200 <= sic <= 3569 or 3580 <= sic <= 3629 or 
          3700 <= sic <= 3709 or 3712 <= sic <= 3713 or 3715 <= sic <= 3715 or 
          3717 <= sic <= 3749 or 3752 <= sic <= 3791 or 3793 <= sic <= 3799 or 
          3830 <= sic <= 3839 or 3860 <= sic <= 3899):
        return 'Manuf'
    
    # 4 Enrgy - Oil, Gas, and Coal Extraction and Products
    elif (1200 <= sic <= 1399 or 2900 <= sic <= 2999):
        return 'Enrgy'
    
    # 5 Chems - Chemicals and Allied Products
    elif (2800 <= sic <= 2829 or 2840 <= sic <= 2899):
        return 'Chems'
    
    # 6 BusEq - Business Equipment
    elif (3570 <= sic <= 3579 or 3660 <= sic <= 3692 or 3694 <= sic <= 3699 or 
          3810 <= sic <= 3829 or 7370 <= sic <= 7379):
        return 'BusEq'
    
    # 7 Telcm - Telephone and Television Transmission
    elif (4800 <= sic <= 4899):
        return 'Telcm'
    
    # 8 Utils - Utilities
    elif (4900 <= sic <= 4949):
        return 'Utils'
    
    # 9 Shops - Wholesale, Retail, and Some Services
    elif (5000 <= sic <= 5999 or 7200 <= sic <= 7299 or 7600 <= sic <= 7699):
        return 'Shops'
    
    # 10 Hlth - Healthcare, Medical Equipment, and Drugs
    elif (2830 <= sic <= 2839 or 3693 <= sic <= 3693 or 3840 <= sic <= 3859 or 
          8000 <= sic <= 8099):
        return 'Hlth'
    
    # 11 Money - Finance
    elif (6000 <= sic <= 6999):
        return 'Money'
    
    # 12 Other - Other industries
    else:
        return 'Other'


def clean_data(df):
    """Clean and prepare data for analysis"""
    print("Cleaning data...")
    
    # Convert numeric columns
    numeric_cols = ['facility_amount', 'maturity_months', 'interest_spread', 
                   'interest_spread_lowest', 'interest_spread_highest',
                   'clean_interest_spread', 'clean_interest_spread_lowest', 'clean_interest_spread_highest']
    
    for col in numeric_cols:
        if col in df.columns:
            df[col] = pd.to_numeric(df[col], errors='coerce')
    
    # Clean facility_type
    if 'facility_type' in df.columns:
        df['facility_type'] = df['facility_type'].str.strip().str.lower()
        # Standardize facility types
        df.loc[df['facility_type'].str.contains('term', na=False), 'facility_type'] = 'term loan'
        df.loc[df['facility_type'].str.contains('revolv', na=False), 'facility_type'] = 'revolver'
        df.loc[df['facility_type'].str.contains('credit', na=False), 'facility_type'] = 'revolver'
    
    # Clean lender_type
    if 'lender_type' in df.columns:
        df['lender_type'] = df['lender_type'].str.strip()
    
    # Add Fama-French 12 industry classification
    if 'sic' in df.columns:
        print("Adding Fama-French 12 industry classifications...")
        df['ff_12_industry'] = df['sic'].apply(get_fama_french_12_industry)
        print(f"Industry distribution: {df['ff_12_industry'].value_counts().to_dict()}")
    else:
        print("Warning: SIC column not found, skipping industry classification")
    
    return df


def create_accession_level_dataset(df):
    """Create accession-level (deal-level) aggregated dataset"""
    print("Creating accession-level aggregated dataset...")
    
    # Debug: Check interest spread data availability
    print(f"Interest spread data check:")
    print(f"  - interest_spread column exists: {'interest_spread' in df.columns}")
    print(f"  - clean_interest_spread column exists: {'clean_interest_spread' in df.columns}")
    if 'interest_spread' in df.columns:
        print(f"  - interest_spread non-null count: {df['interest_spread'].notna().sum()}")
        print(f"  - interest_spread sample values: {df['interest_spread'].dropna().head().tolist()}")
    if 'clean_interest_spread' in df.columns:
        print(f"  - clean_interest_spread non-null count: {df['clean_interest_spread'].notna().sum()}")
        print(f"  - clean_interest_spread sample values: {df['clean_interest_spread'].dropna().head().tolist()}")
    
    # Group by accession and aggregate - only include columns that exist
    agg_dict = {
        'facility_amount': 'sum',  # Total deal amount
        'maturity_months': 'mean',  # Average maturity
        'interest_spread': 'mean',  # Average interest spread
        'clean_interest_spread': 'mean',  # Average clean interest spread
        'lender_type': 'first',  # Keep first lender type (assuming consistent within deal)
        'ff_12_industry': 'first',  # Keep first industry (assuming consistent within deal)
        'facility_type': 'first',  # Keep first facility type
        'secured': 'first',  # Keep first secured status
        'borrower_name': 'first'  # Keep borrower name
    }
    
    # Add year if it exists
    if 'year' in df.columns:
        agg_dict['year'] = 'first'
    
    # Filter agg_dict to only include columns that exist in df
    existing_cols = {col: func for col, func in agg_dict.items() if col in df.columns}
    
    print(f"Columns to aggregate: {list(existing_cols.keys())}")
    
    # Create aggregated dataset
    df_accession = df.groupby('accession').agg(existing_cols).reset_index()
    
    # Debug: Check aggregated interest spread data
    if 'interest_spread' in df_accession.columns:
        print(f"  - Aggregated interest_spread non-null count: {df_accession['interest_spread'].notna().sum()}")
        print(f"  - Aggregated interest_spread sample values: {df_accession['interest_spread'].dropna().head().tolist()}")
    if 'clean_interest_spread' in df_accession.columns:
        print(f"  - Aggregated clean_interest_spread non-null count: {df_accession['clean_interest_spread'].notna().sum()}")
        print(f"  - Aggregated clean_interest_spread sample values: {df_accession['clean_interest_spread'].dropna().head().tolist()}")
    
    print(f"Accession-level dataset: {len(df_accession)} deals from {len(df)} facilities")
    
    return df_accession


def generate_summary_stats(df, output_dir):
    """Generate summary statistics by facility type and lender type"""
    print("Generating summary statistics...")
    
    # Overall summary
    summary_stats = []
    
    # Key variables to analyze
    key_vars = ['facility_amount', 'maturity_months', 'interest_spread', 'clean_interest_spread']
    
    # Overall statistics
    for var in key_vars:
        if var in df.columns:
            stats = df[var].describe()
            summary_stats.append({
                'group': 'Overall',
                'variable': var,
                'count': stats['count'],
                'mean': stats['mean'],
                'std': stats['std'],
                'min': stats['min'],
                '25%': stats['25%'],
                '50%': stats['50%'],
                '75%': stats['75%'],
                'max': stats['max']
            })
    
    # By facility type
    if 'facility_type' in df.columns:
        for facility_type in df['facility_type'].dropna().unique():
            if facility_type in ['term loan', 'revolver']:
                subset = df[df['facility_type'] == facility_type]
                for var in key_vars:
                    if var in df.columns:
                        stats = subset[var].describe()
                        summary_stats.append({
                            'group': f'Facility: {facility_type.title()}',
                            'variable': var,
                            'count': stats['count'],
                            'mean': stats['mean'],
                            'std': stats['std'],
                            'min': stats['min'],
                            '25%': stats['25%'],
                            '50%': stats['50%'],
                            '75%': stats['75%'],
                            'max': stats['max']
                        })
    
    # By lender type
    if 'lender_type' in df.columns:
        for lender_type in df['lender_type'].dropna().unique():
            subset = df[df['lender_type'] == lender_type]
            for var in key_vars:
                if var in df.columns and len(subset) >= 5:  # Only if at least 5 observations
                    stats = subset[var].describe()
                    summary_stats.append({
                        'group': f'Lender: {lender_type}',
                        'variable': var,
                        'count': stats['count'],
                        'mean': stats['mean'],
                        'std': stats['std'],
                        'min': stats['min'],
                        '25%': stats['25%'],
                        '50%': stats['50%'],
                        '75%': stats['75%'],
                        'max': stats['max']
                    })
    
    # By Fama-French 12 industry
    if 'ff_12_industry' in df.columns:
        for industry in df['ff_12_industry'].dropna().unique():
            subset = df[df['ff_12_industry'] == industry]
            for var in key_vars:
                if var in df.columns and len(subset) >= 5:  # Only if at least 5 observations
                    stats = subset[var].describe()
                    summary_stats.append({
                        'group': f'Industry: {industry}',
                        'variable': var,
                        'count': stats['count'],
                        'mean': stats['mean'],
                        'std': stats['std'],
                        'min': stats['min'],
                        '25%': stats['25%'],
                        '50%': stats['50%'],
                        '75%': stats['75%'],
                        'max': stats['max']
                    })
    
    # Convert to DataFrame and save
    summary_df = pd.DataFrame(summary_stats)
    summary_file = output_dir / "5b_DescriptiveStats_NonbankLoans.csv"
    summary_df.to_csv(summary_file, index=False)
    print(f"Summary statistics saved to: {summary_file}")
    
    return summary_df


def create_histograms(df, output_dir):
    """Create interactive histograms using Plotly"""
    print("Creating histograms...")
    
    # Variables to plot - use clean_interest_spread if interest_spread is not available
    spread_col = 'clean_interest_spread' if 'clean_interest_spread' in df.columns and df['clean_interest_spread'].notna().sum() > 0 else 'interest_spread'
    
    variables = {
        'facility_amount': {'title': 'Facility Amount (USD)', 'log_scale': True},
        'maturity_months': {'title': 'Maturity (Months)', 'log_scale': False},
        spread_col: {'title': 'Interest Spread (bps)', 'log_scale': False}
    }
    
    print(f"Using {spread_col} for facility-level interest spread histograms")
    
    # Create subplot titles
    subplot_titles = []
    for var, var_info in variables.items():
        subplot_titles.extend([f"{var_info['title']} - Term Loans", f"{var_info['title']} - Revolvers"])
    
    # Create subplots
    fig = make_subplots(
        rows=len(variables), cols=2,
        subplot_titles=subplot_titles,
        vertical_spacing=0.08
    )
    
    colors = px.colors.qualitative.Set1
    
    for i, (var, var_info) in enumerate(variables.items(), 1):
        if var not in df.columns:
            continue
            
        # Filter out missing values
        data_clean = df[df[var].notna()]
        
        if len(data_clean) == 0:
            continue
        
        # Term loans
        term_data = data_clean[data_clean['facility_type'] == 'term loan'][var]
        if len(term_data) > 0:
            fig.add_trace(
                go.Histogram(
                    x=term_data,
                    name=f'Term Loans',
                    marker_color=colors[0],
                    opacity=0.7,
                    nbinsx=30
                ),
                row=i, col=1
            )
        
        # Revolvers
        revolver_data = data_clean[data_clean['facility_type'] == 'revolver'][var]
        if len(revolver_data) > 0:
            fig.add_trace(
                go.Histogram(
                    x=revolver_data,
                    name=f'Revolvers',
                    marker_color=colors[1],
                    opacity=0.7,
                    nbinsx=30
                ),
                row=i, col=2
            )
    
    # Update layout
    fig.update_layout(
        title_text="Nonbank Loans: Distribution by Facility Type",
        title_x=0.5,
        height=800,
        showlegend=False,
        font=dict(size=12)
    )
    
    # Save histogram as PNG
    hist_file = output_dir / "5b_Histograms_FacilityType_NonbankLoans.png"
    pio.write_image(fig, str(hist_file), width=1200, height=800, scale=2)
    print(f"Facility type histograms saved to: {hist_file}")
    
    # Create lender type histograms
    create_lender_type_histograms(df, output_dir)


def create_lender_type_histograms(df, output_dir):
    """Create histograms by lender type"""
    print("Creating lender type histograms...")
    
    # Get top lender types by count
    lender_counts = df['lender_type'].value_counts()
    top_lenders = lender_counts.head(6).index.tolist()  # Top 6 lender types
    
    # Use clean_interest_spread if interest_spread is not available
    spread_col = 'clean_interest_spread' if 'clean_interest_spread' in df.columns and df['clean_interest_spread'].notna().sum() > 0 else 'interest_spread'
    
    variables = {
        'facility_amount': {'title': 'Facility Amount (USD)', 'log_scale': True},
        'maturity_months': {'title': 'Maturity (Months)', 'log_scale': False},
        spread_col: {'title': 'Interest Spread (bps)', 'log_scale': False}
    }
    
    print(f"Using {spread_col} for facility-level interest spread histograms by lender type")
    
    for var, var_info in variables.items():
        if var not in df.columns:
            continue
        
        # Create subplot for this variable
        fig = make_subplots(
            rows=2, cols=3,
            subplot_titles=[lender.replace(' / ', '/') for lender in top_lenders],
            vertical_spacing=0.1
        )
        
        colors = px.colors.qualitative.Set3
        
        for i, lender_type in enumerate(top_lenders):
            row = (i // 3) + 1
            col = (i % 3) + 1
            
            lender_data = df[df['lender_type'] == lender_type][var].dropna()
            
            if len(lender_data) > 0:
                fig.add_trace(
                    go.Histogram(
                        x=lender_data,
                        name=lender_type,
                        marker_color=colors[i % len(colors)],
                        opacity=0.7,
                        nbinsx=20,
                        showlegend=False
                    ),
                    row=row, col=col
                )
        
        fig.update_layout(
            title_text=f"Nonbank Loans: {var_info['title']} by Lender Type",
            title_x=0.5,
            height=600,
            font=dict(size=12)
        )
        
        # Save individual histogram as PNG
        safe_var_name = var.replace('_', '').lower()
        hist_file = output_dir / f"5b_Histogram_{safe_var_name}_by_LenderType.png"
        pio.write_image(fig, str(hist_file), width=1200, height=600, scale=2)
        print(f"{var} by lender type histogram saved to: {hist_file}")


def generate_cross_tabulations(df, output_dir):
    """Generate cross-tabulation heatmaps"""
    print("Generating cross-tabulation heatmaps...")
    
    # Set up matplotlib style
    plt.style.use('default')
    sns.set_palette("husl")
    
    # Facility type vs Lender type heatmap
    if 'facility_type' in df.columns and 'lender_type' in df.columns:
        ct1 = pd.crosstab(df['facility_type'], df['lender_type'])
        
        fig, ax = plt.subplots(figsize=(12, 6))
        sns.heatmap(ct1, annot=True, fmt='d', cmap='Blues', ax=ax)
        ax.set_title('Facility Type vs Lender Type', fontsize=14, fontweight='bold')
        ax.set_xlabel('Lender Type', fontsize=12)
        ax.set_ylabel('Facility Type', fontsize=12)
        plt.xticks(rotation=45, ha='right')
        plt.tight_layout()
        
        ct1_file = output_dir / "5b_Heatmap_FacilityType_vs_LenderType.png"
        plt.savefig(ct1_file, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Facility Type vs Lender Type heatmap saved to: {ct1_file}")
    
    # Secured vs Lender type heatmap
    if 'secured' in df.columns and 'lender_type' in df.columns:
        ct2 = pd.crosstab(df['secured'], df['lender_type'])
        
        fig, ax = plt.subplots(figsize=(12, 6))
        sns.heatmap(ct2, annot=True, fmt='d', cmap='Reds', ax=ax)
        ax.set_title('Secured vs Lender Type', fontsize=14, fontweight='bold')
        ax.set_xlabel('Lender Type', fontsize=12)
        ax.set_ylabel('Secured', fontsize=12)
        plt.xticks(rotation=45, ha='right')
        plt.tight_layout()
        
        ct2_file = output_dir / "5b_Heatmap_Secured_vs_LenderType.png"
        plt.savefig(ct2_file, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Secured vs Lender Type heatmap saved to: {ct2_file}")
    
    # Industry vs Lender type heatmap
    if 'ff_12_industry' in df.columns and 'lender_type' in df.columns:
        ct3 = pd.crosstab(df['ff_12_industry'], df['lender_type'])
        
        fig, ax = plt.subplots(figsize=(14, 8))
        sns.heatmap(ct3, annot=True, fmt='d', cmap='Greens', ax=ax)
        ax.set_title('Fama-French 12 Industry vs Lender Type', fontsize=14, fontweight='bold')
        ax.set_xlabel('Lender Type', fontsize=12)
        ax.set_ylabel('Industry', fontsize=12)
        plt.xticks(rotation=45, ha='right')
        plt.yticks(rotation=0)
        plt.tight_layout()
        
        ct3_file = output_dir / "5b_Heatmap_Industry_vs_LenderType.png"
        plt.savefig(ct3_file, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Industry vs Lender Type heatmap saved to: {ct3_file}")
    
    # Industry distribution bar chart
    if 'ff_12_industry' in df.columns:
        industry_counts = df['ff_12_industry'].value_counts()
        
        fig, ax = plt.subplots(figsize=(12, 6))
        bars = ax.bar(range(len(industry_counts)), industry_counts.values, color='skyblue')
        ax.set_title('Number of Loans by Fama-French 12 Industry', fontsize=14, fontweight='bold')
        ax.set_xlabel('Industry', fontsize=12)
        ax.set_ylabel('Number of Loans', fontsize=12)
        ax.set_xticks(range(len(industry_counts)))
        ax.set_xticklabels(industry_counts.index, rotation=45, ha='right')
        
        # Add value labels on bars
        for bar, count in zip(bars, industry_counts.values):
            ax.text(bar.get_x() + bar.get_width()/2, bar.get_height() + 0.5, 
                   str(count), ha='center', va='bottom', fontsize=10)
        
        plt.tight_layout()
        
        industry_file = output_dir / "5b_BarChart_IndustryDistribution.png"
        plt.savefig(industry_file, dpi=300, bbox_inches='tight')
        plt.close()
        print(f"Industry distribution bar chart saved to: {industry_file}")
        
        # Print industry summary
        print(f"\nIndustry Distribution Summary:")
        for industry, count in industry_counts.items():
            print(f"  {industry}: {count} loans ({count/len(df)*100:.1f}%)")


def generate_deal_level_analysis(df_accession, output_dir):
    """Generate deal-level (accession-level) analysis"""
    print("Generating deal-level analysis...")
    
    # 1. Number of deals by industry
    print("\n=== DEAL-LEVEL ANALYSIS ===")
    print(f"Total deals: {len(df_accession)}")
    
    if 'ff_12_industry' in df_accession.columns:
        industry_deal_counts = df_accession['ff_12_industry'].value_counts()
        print(f"\nNumber of deals by industry:")
        for industry, count in industry_deal_counts.items():
            print(f"  {industry}: {count} deals ({count/len(df_accession)*100:.1f}%)")
        
        # Save deal-level industry statistics
        industry_deal_df = pd.DataFrame({
            'industry': industry_deal_counts.index,
            'deal_count': industry_deal_counts.values,
            'deal_percentage': (industry_deal_counts.values / len(df_accession) * 100).round(1)
        })
        industry_file = output_dir / "5b_DealLevel_IndustryStatistics.csv"
        industry_deal_df.to_csv(industry_file, index=False)
        print(f"Deal-level industry statistics saved to: {industry_file}")
    
    # 2. Create deal-level histograms
    create_deal_level_histograms(df_accession, output_dir)
    
    # 3. Create deal-level histograms by lender type
    create_deal_level_lender_histograms(df_accession, output_dir)


def create_deal_level_histograms(df_accession, output_dir):
    """Create histograms for deal-level data"""
    print("Creating deal-level histograms...")
    
    # Variables to plot - use clean_interest_spread if interest_spread is not available
    spread_col = 'clean_interest_spread' if 'clean_interest_spread' in df_accession.columns and df_accession['clean_interest_spread'].notna().sum() > 0 else 'interest_spread'
    
    variables = {
        'facility_amount': {'title': 'Deal Amount (USD)', 'log_scale': True},
        'maturity_months': {'title': 'Average Maturity (Months)', 'log_scale': False},
        spread_col: {'title': 'Average Interest Spread (bps)', 'log_scale': False}
    }
    
    print(f"Using {spread_col} for interest spread histogram")
    
    # Create subplots
    fig = make_subplots(
        rows=1, cols=3,
        subplot_titles=[var_info['title'] for var, var_info in variables.items()],
        vertical_spacing=0.1
    )
    
    for i, (var, var_info) in enumerate(variables.items(), 1):
        if var not in df_accession.columns:
            print(f"Warning: Column {var} not found in deal-level dataset")
            continue
            
        # Filter out missing values
        data_clean = df_accession[df_accession[var].notna()]
        
        print(f"Creating histogram for {var}: {len(data_clean)} non-null values")
        
        if len(data_clean) == 0:
            print(f"Warning: No data for {var}")
            continue
        
        # Remove extreme outliers for better visualization
        if var == 'facility_amount':
            # Remove top 1% outliers for facility amount
            threshold = data_clean[var].quantile(0.99)
            data_clean = data_clean[data_clean[var] <= threshold]
        elif var == 'interest_spread':
            # Remove extreme interest spreads
            data_clean = data_clean[(data_clean[var] >= 0) & (data_clean[var] <= 2000)]
        
        fig.add_trace(
            go.Histogram(
                x=data_clean[var],
                name=var_info['title'],
                marker_color=px.colors.qualitative.Set1[i-1],
                opacity=0.7,
                nbinsx=30,
                showlegend=False
            ),
            row=1, col=i
        )
    
    # Update layout
    fig.update_layout(
        title_text="Nonbank Loans: Deal-Level Distributions",
        title_x=0.5,
        height=500,
        font=dict(size=12)
    )
    
    # Save histogram
    hist_file = output_dir / "5b_DealLevel_Histograms.png"
    pio.write_image(fig, str(hist_file), width=1500, height=500, scale=2)
    print(f"Deal-level histograms saved to: {hist_file}")


def create_deal_level_lender_histograms(df_accession, output_dir):
    """Create deal-level histograms by lender type"""
    print("Creating deal-level histograms by lender type...")
    
    # Get top lender types by count
    lender_counts = df_accession['lender_type'].value_counts()
    top_lenders = lender_counts.head(6).index.tolist()  # Top 6 lender types
    
    # Use clean_interest_spread if interest_spread is not available
    spread_col = 'clean_interest_spread' if 'clean_interest_spread' in df_accession.columns and df_accession['clean_interest_spread'].notna().sum() > 0 else 'interest_spread'
    
    variables = {
        'facility_amount': {'title': 'Deal Amount (USD)', 'log_scale': True},
        'maturity_months': {'title': 'Average Maturity (Months)', 'log_scale': False},
        spread_col: {'title': 'Average Interest Spread (bps)', 'log_scale': False}
    }
    
    print(f"Using {spread_col} for interest spread histograms by lender type")
    
    for var, var_info in variables.items():
        if var not in df_accession.columns:
            continue
        
        # Create subplot for this variable
        fig = make_subplots(
            rows=2, cols=3,
            subplot_titles=[lender.replace(' / ', '/') for lender in top_lenders],
            vertical_spacing=0.1
        )
        
        colors = px.colors.qualitative.Set3
        
        for i, lender_type in enumerate(top_lenders):
            row = (i // 3) + 1
            col = (i % 3) + 1
            
            lender_data = df_accession[df_accession['lender_type'] == lender_type][var].dropna()
            
            # Remove outliers for better visualization
            if var == 'interest_spread' and len(lender_data) > 0:
                lender_data = lender_data[(lender_data >= 0) & (lender_data <= 2000)]
            elif var == 'facility_amount' and len(lender_data) > 0:
                threshold = lender_data.quantile(0.99)
                lender_data = lender_data[lender_data <= threshold]
            
            print(f"  {lender_type}: {len(lender_data)} deals for {var}")
            
            if len(lender_data) > 0:
                fig.add_trace(
                    go.Histogram(
                        x=lender_data,
                        name=lender_type,
                        marker_color=colors[i % len(colors)],
                        opacity=0.7,
                        nbinsx=20,
                        showlegend=False
                    ),
                    row=row, col=col
                )
        
        fig.update_layout(
            title_text=f"Nonbank Loans: Deal-Level {var_info['title']} by Lender Type",
            title_x=0.5,
            height=600,
            font=dict(size=12)
        )
        
        # Save individual histogram
        safe_var_name = var.replace('_', '').lower()
        hist_file = output_dir / f"5b_DealLevel_{safe_var_name}_by_LenderType.png"
        pio.write_image(fig, str(hist_file), width=1200, height=600, scale=2)
        print(f"Deal-level {var} by lender type histogram saved to: {hist_file}")


def main():
    print("=" * 80)
    print("5b_DescribePanelNonbankLoans.py - Descriptive Statistics and Visualizations")
    print("=" * 80)
    
    # Set up paths
    script_dir = Path(__file__).parent
    input_csv = script_dir / ".." / "Data" / "Intermediate" / "5a_PanelNonbankLoans.csv"
    output_dir = script_dir / ".." / "Data" / "Intermediate"  # For summary CSV
    figures_dir = Path("/Users/zrsong/MIT Dropbox/Zirui Song/Research Projects/PSW_Nonbank Direct Lending/Results/Figures")
    
    # Create figures directory if it doesn't exist
    figures_dir.mkdir(parents=True, exist_ok=True)
    
    # Check if input file exists
    if not input_csv.exists():
        print(f"Error: Input file not found: {input_csv}")
        return
    
    # Load data
    print("Loading panel dataset...")
    try:
        df = pd.read_csv(input_csv)
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
    if 'facility_type' in df.columns:
        print(f"Facility types: {df['facility_type'].value_counts().to_dict()}")
    if 'lender_type' in df.columns:
        print(f"Lender types: {len(df['lender_type'].unique())}")
    
    # Create accession-level (deal-level) dataset
    df_accession = create_accession_level_dataset(df)
    
    # Generate summary statistics
    summary_df = generate_summary_stats(df, output_dir)
    
    # Create histograms
    create_histograms(df, figures_dir)
    
    # Generate cross-tabulations
    generate_cross_tabulations(df, figures_dir)
    
    # Generate deal-level analysis
    generate_deal_level_analysis(df_accession, figures_dir)
    
    print(f"\n" + "=" * 80)
    print("DESCRIPTIVE ANALYSIS COMPLETE")
    print("=" * 80)
    print(f"Summary statistics: {output_dir}")
    print(f"Figures directory: {figures_dir}")
    print(f"Files generated:")
    print(f"  - 5b_DescriptiveStats_NonbankLoans.csv (summary statistics)")
    print(f"  - 5b_Histograms_FacilityType_NonbankLoans.png (facility type comparisons)")
    print(f"  - 5b_Histogram_facilityamount_by_LenderType.png (loan amounts by lender)")
    print(f"  - 5b_Histogram_maturitymonths_by_LenderType.png (maturity by lender)")
    print(f"  - 5b_Histogram_cleaninterestspread_by_LenderType.png (pricing by lender)")
    print(f"  - 5b_Heatmap_FacilityType_vs_LenderType.png (facility preferences)")
    print(f"  - 5b_Heatmap_Secured_vs_LenderType.png (security requirements)")
    print(f"  - 5b_Heatmap_Industry_vs_LenderType.png (industry preferences)")
    print(f"  - 5b_BarChart_IndustryDistribution.png (industry loan counts)")
    print(f"  - 5b_DealLevel_IndustryStatistics.csv (deal counts by industry)")
    print(f"  - 5b_DealLevel_Histograms.png (deal-level distributions)")
    print(f"  - 5b_DealLevel_facilityamount_by_LenderType.png (deal amounts by lender)")
    print(f"  - 5b_DealLevel_maturitymonths_by_LenderType.png (deal maturity by lender)")
    print(f"  - 5b_DealLevel_interestspread_by_LenderType.png (deal pricing by lender)")


if __name__ == "__main__":
    main()
