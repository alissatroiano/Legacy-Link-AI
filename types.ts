
// Bailey the Bunny says: Hello there, little architect! Let's make sure our laces are tied tight and our types match perfectly!
// Renaming originalName to name so we can follow the trail of our COBOL code just like a bunny trail! Hop, hop, hooray!

export type ModernizationStrategy = 'rehosting' | 'refactoring' | 'hybrid';

export interface CodeChunk {
  id: string;
  /* Bailey the Bunny: Renamed to match the API response and fix the errors! */
  name: string;
  cobolCode: string;
  // Refactoring outputs
  pythonCode?: string;
  unitTests?: string;
  // Rehosting outputs
  terraform?: string;
  startup?: string;
  recommendedInstance?: string;
  
  status: 'pending' | 'analyzing' | 'processing' | 'completed' | 'error';
  progress: number;
  errorMessage?: string;
}

export enum ConversionStep {
  IDLE = 'IDLE',
  ANALYZING_SYSTEM = 'ANALYZING_SYSTEM',
  CONVERTING_INCREMENTS = 'CONVERTING_INCREMENTS',
  FINALIZING = 'FINALIZING'
}

export interface GitHubUser {
  id: string;
  login: string;
  name: string;
  avatar_url: string;
  role: string;
}

export interface GitHubConfig {
  repo: string;
  branch: string;
  commitMessage: string;
  isPrivate: boolean;
}
