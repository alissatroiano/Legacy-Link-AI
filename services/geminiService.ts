
import { GoogleGenAI, Type } from "@google/genai";

const ai = new GoogleGenAI({ apiKey: process.env.API_KEY });

export const geminiService = {
  /**
   * Unified analysis of COBOL for both Infrastructure (GCP) and Code (Python).
   */
  async analyzeSystem(cobolCode: string, fileName: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-pro-preview",
      contents: `
        Analyze this COBOL code for a dual modernization strategy:
        1. GCE Rehosting: Identify instance requirements and runtime dependencies.
        2. Python Refactoring: Break code into logical modules for translation.
        
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
              name: { type: Type.STRING },
              code: { type: Type.STRING },
              recommendedInstance: { type: Type.STRING },
              refactorComplexity: { type: Type.STRING }
            },
            required: ["name", "code", "recommendedInstance"]
          }
        }
      }
    });

    try {
      return JSON.parse(response.text || "[]");
    } catch (e) {
      throw new Error("Failed to parse system analysis.");
    }
  },

  /**
   * Processes a single increment, generating both Python code and Terraform IaC.
   */
  async processIncrement(cobolSnippet: string, instanceType: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-pro-preview",
      contents: `
        Modernize this COBOL unit.
        
        OUTPUT 1: Python 3.12 conversion with functional parity.
        OUTPUT 2: Terraform code for Google Compute Engine (n2-standard-4) with Micro Focus runtime.
        
        COBOL:
        ${cobolSnippet}
      `,
      config: {
        responseMimeType: "application/json",
        responseSchema: {
          type: Type.OBJECT,
          properties: {
            pythonCode: { type: Type.STRING },
            unitTests: { type: Type.STRING },
            terraform: { type: Type.STRING },
            startup: { type: Type.STRING }
          },
          required: ["pythonCode", "terraform"]
        }
      }
    });

    try {
      return JSON.parse(response.text || "{}");
    } catch (e) {
      throw new Error("Failed to process modernization increment.");
    }
  }
};
