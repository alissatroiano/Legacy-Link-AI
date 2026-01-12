
import { GoogleGenAI, Type } from "@google/genai";

const ai = new GoogleGenAI({ apiKey: process.env.API_KEY });

export const geminiService = {
  /**
   * Analyzes COBOL source to determine infrastructure requirements for GCE rehosting.
   */
  async analyzeForCloud(cobolCode: string, fileName: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-pro-preview",
      contents: `
        You are a Google Cloud Solutions Architect specializing in Mainframe Modernization.
        Analyze this COBOL code to determine its rehosting requirements on Google Compute Engine (GCE).
        
        TASKS:
        1. Break the code into logical Migration Waves (increments).
        2. Identify required runtime environment (e.g., Micro Focus Enterprise Server, Rocket Enterprise Server).
        3. Estimate CPU/Memory requirements based on paragraph complexity and DATA DIVISION definitions.
        
        File: ${fileName}
        Code:
        ${cobolCode}
      `,
      config: {
        responseMimeType: "application/json",
        responseSchema: {
          type: Type.ARRAY,
          items: {
            type: Type.OBJECT,
            properties: {
              name: { type: Type.STRING, description: "Name of the component/paragraph" },
              code: { type: Type.STRING, description: "The original COBOL segment" },
              requirement: { type: Type.STRING, description: "Specific rehosting requirement (e.g. VSAM mapping, JCL dependency)" },
              recommendedInstance: { type: Type.STRING, description: "Recommended GCE Machine Type (e.g. n2-standard-4)" }
            },
            required: ["name", "code", "requirement"]
          }
        }
      }
    });

    try {
      const text = response.text;
      if (!text) throw new Error("Empty response from Gemini API");
      return JSON.parse(text.trim());
    } catch (e) {
      console.error("Failed to parse cloud analysis response", e);
      throw new Error("Could not parse COBOL cloud analysis results.");
    }
  },

  /**
   * Generates Terraform (IaC) or Deployment configurations for the GCE host.
   */
  async generateCloudConfig(cobolSnippet: string, instanceType: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-pro-preview",
      contents: `
        Generate a Google Cloud Terraform configuration (IaC) and an initialization script (cloud-init) for rehosting the following COBOL workload on a GCE instance.
        
        SPECIFICATIONS:
        - Target: Google Compute Engine (GCE)
        - Runtime: Micro Focus Enterprise Server
        - Instance Type: ${instanceType || 'n2-standard-2'}
        - Region: us-central1
        
        Include:
        1. Terraform resource "google_compute_instance" definition.
        2. Startup script to install the partner runtime and mount disks.
        3. Firewall rules for administrative ports.
        
        COBOL CONTEXT:
        ${cobolSnippet}
      `,
      config: {
        responseMimeType: "application/json",
        responseSchema: {
          type: Type.OBJECT,
          properties: {
            terraformConfig: { type: Type.STRING, description: "The HCL Terraform code" },
            startupScript: { type: Type.STRING, description: "The bash shell script for GCE metadata" },
            architectureNotes: { type: Type.STRING, description: "Brief explanation of the GCE architecture choices" }
          },
          required: ["terraformConfig", "startupScript"]
        }
      }
    });

    try {
      const text = response.text;
      if (!text) throw new Error("Empty response from Gemini API");
      return JSON.parse(text.trim());
    } catch (e) {
      return { 
        terraformConfig: "# Error generating IaC", 
        startupScript: "", 
        architectureNotes: "API parse error" 
      };
    }
  },

  /**
   * Summarizes the rehosting readiness.
   */
  async generateReadinessReport(completedCount: number, totalCount: number, latestUpdates: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-flash-preview",
      contents: `Generate a brief, professional GCP rehosting readiness summary. 
      Provisioned: ${completedCount} of ${totalCount} workloads. 
      Latest Activity: ${latestUpdates}. Target: Compute Engine.`
    });
    return response.text || "";
  }
};
