
import { GoogleGenAI, Type } from "@google/genai";

const ai = new GoogleGenAI({ apiKey: process.env.API_KEY || '' });

export const geminiService = {
  /**
   * Analyzes a large COBOL file and breaks it into logical increments.
   */
  async analyzeAndChunk(cobolCode: string, fileName: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-pro-preview",
      contents: `
        Analyze this COBOL code and break it down into logical, migratable increments (chunks).
        The original code has approximately ${cobolCode.split('\n').length} lines.
        Each chunk should represent a meaningful module, paragraph, or section (e.g., DATA DIVISION, specific paragraphs).
        
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
              name: { type: Type.STRING, description: "Name of the logical segment" },
              code: { type: Type.STRING, description: "The specific COBOL snippet for this chunk" },
              description: { type: Type.STRING, description: "Short description of what this chunk does" }
            },
            required: ["name", "code"]
          }
        }
      }
    });

    try {
      return JSON.parse(response.text);
    } catch (e) {
      console.error("Failed to parse analysis response", e);
      throw new Error("Could not parse COBOL analysis results.");
    }
  },

  /**
   * Converts a specific COBOL chunk to Python.
   */
  async convertChunkToPython(cobolSnippet: string, context: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-pro-preview",
      contents: `
        You are a senior banking systems architect specializing in COBOL to Python migration.
        Convert the following COBOL snippet into clean, PEP8 compliant, modular, and maintainable Python code.
        
        CONTEXT OF PREVIOUS WORK: ${context}
        
        REQUIREMENTS:
        1. Use Object-Oriented patterns where appropriate.
        2. Map COBOL data types (e.g. PIC X, PIC 9) to appropriate Python types or Pydantic models.
        3. Handle legacy error handling with modern Python exceptions.
        4. Maintain functional parity with the original banking logic.
        5. Provide ONLY the Python code and a small block of unit tests using pytest.
        
        COBOL SNIPPET:
        ${cobolSnippet}
      `,
      config: {
        responseMimeType: "application/json",
        responseSchema: {
          type: Type.OBJECT,
          properties: {
            pythonCode: { type: Type.STRING },
            unitTests: { type: Type.STRING },
            explanation: { type: Type.STRING }
          },
          required: ["pythonCode", "unitTests"]
        }
      }
    });

    try {
      return JSON.parse(response.text);
    } catch (e) {
      return { 
        pythonCode: "# Error converting snippet", 
        unitTests: "", 
        explanation: "API parse error" 
      };
    }
  },

  /**
   * Summarizes the entire modernization progress.
   */
  async generateStatusUpdate(completedCount: number, totalCount: number, latestUpdates: string) {
    const response = await ai.models.generateContent({
      model: "gemini-3-flash-preview",
      contents: `Generate a brief, professional executive summary of a banking system migration. 
      Completed: ${completedCount} of ${totalCount} modules. 
      Latest Activity: ${latestUpdates}`
    });
    return response.text;
  }
};
