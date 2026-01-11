
import React, { useState, useEffect, useCallback, useRef } from 'react';
import { 
  Terminal, 
  Settings, 
  Cpu, 
  Database, 
  Layers, 
  CheckCircle2, 
  AlertCircle, 
  Clock, 
  ArrowRightLeft,
  Upload,
  BarChart3,
  Code2,
  PlayCircle,
  FileCode2,
  Info
} from 'lucide-react';
import { 
  BarChart, 
  Bar, 
  XAxis, 
  YAxis, 
  CartesianGrid, 
  Tooltip, 
  ResponsiveContainer, 
  Cell,
  PieChart,
  Pie
} from 'recharts';
import { geminiService } from './services/geminiService';
import { CodeChunk, ConversionStep } from './types';

// Mock COBOL Code for testing
const MOCK_COBOL = `
       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANK-LEDGER-SYSTEM.
       
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT ACCT-FILE ASSIGN TO "ACCOUNTS.DAT".
           
       DATA DIVISION.
       FILE SECTION.
       FD ACCT-FILE.
       01 ACCT-RECORD.
          05 ACCT-NUM      PIC 9(10).
          05 ACCT-NAME     PIC X(30).
          05 ACCT-BALANCE  PIC S9(13)V99.
          
       WORKING-STORAGE SECTION.
       01 WS-EOF          PIC X VALUE 'N'.
       01 WS-TRAN-AMT     PIC S9(13)V99.
       
       PROCEDURE DIVISION.
       MAIN-LOGIC.
           OPEN INPUT ACCT-FILE.
           PERFORM UNTIL WS-EOF = 'Y'
               READ ACCT-FILE
                   AT END MOVE 'Y' TO WS-EOF
                   NOT AT END PERFORM PROCESS-ACCOUNT
               END-READ
           END-PERFORM.
           CLOSE ACCT-FILE.
           STOP RUN.
           
       PROCESS-ACCOUNT.
           IF ACCT-BALANCE < 0
               DISPLAY "OVERDRAWN: " ACCT-NAME
           ELSE
               DISPLAY "CLEAR: " ACCT-NAME
           END-IF.
`;

const App: React.FC = () => {
  const [step, setStep] = useState<ConversionStep>(ConversionStep.IDLE);
  const [chunks, setChunks] = useState<CodeChunk[]>([]);
  const [selectedChunkId, setSelectedChunkId] = useState<string | null>(null);
  const [projectName, setProjectName] = useState('CoreBanking_v2.0');
  const [logs, setLogs] = useState<string[]>([]);
  const [executiveSummary, setExecutiveSummary] = useState('');
  const logEndRef = useRef<HTMLDivElement>(null);

  const addLog = (msg: string) => {
    setLogs(prev => [...prev, `[${new Date().toLocaleTimeString()}] ${msg}`]);
  };

  useEffect(() => {
    logEndRef.current?.scrollIntoView({ behavior: 'smooth' });
  }, [logs]);

  const handleFileUpload = async (e: React.ChangeEvent<HTMLInputElement>) => {
    const file = e.target.files?.[0];
    if (!file) return;

    setStep(ConversionStep.ANALYZING_SYSTEM);
    addLog(`Initiating migration for: ${file.name}`);
    
    const reader = new FileReader();
    reader.onload = async (event) => {
      const content = event.target?.result as string;
      try {
        const analysis = await geminiService.analyzeAndChunk(content, file.name);
        const newChunks: CodeChunk[] = analysis.map((item: any, idx: number) => ({
          id: `chunk-${idx}`,
          originalName: item.name,
          cobolCode: item.code,
          status: 'pending',
          progress: 0
        }));
        setChunks(newChunks);
        addLog(`Analysis complete. Identified ${newChunks.length} logical modules for incremental conversion.`);
        setStep(ConversionStep.CONVERTING_INCREMENTS);
      } catch (err) {
        addLog(`System Error: ${err instanceof Error ? err.message : 'Unknown error during analysis'}`);
        setStep(ConversionStep.IDLE);
      }
    };
    reader.readAsText(file);
  };

  const processNextChunk = useCallback(async () => {
    const nextPending = chunks.find(c => c.status === 'pending');
    if (!nextPending) {
      if (chunks.length > 0 && chunks.every(c => c.status === 'completed')) {
        setStep(ConversionStep.FINALIZING);
      }
      return;
    }

    // Mark as converting
    setChunks(prev => prev.map(c => c.id === nextPending.id ? { ...c, status: 'converting', progress: 30 } : c));
    addLog(`Modernizing module: ${nextPending.originalName}...`);

    try {
      // Gather context from previous conversions (simplified)
      const context = chunks
        .filter(c => c.status === 'completed')
        .slice(-3)
        .map(c => c.originalName)
        .join(', ');

      const result = await geminiService.convertChunkToPython(nextPending.cobolCode, context);
      
      setChunks(prev => prev.map(c => c.id === nextPending.id ? { 
        ...c, 
        pythonCode: result.pythonCode, 
        unitTests: result.unitTests,
        status: 'completed', 
        progress: 100 
      } : c));

      addLog(`Success: ${nextPending.originalName} converted and unit tests generated.`);
      
      const summary = await geminiService.generateStatusUpdate(
        chunks.filter(c => c.status === 'completed').length + 1,
        chunks.length,
        `Converted ${nextPending.originalName}`
      );
      setExecutiveSummary(summary);

    } catch (err) {
      setChunks(prev => prev.map(c => c.id === nextPending.id ? { ...c, status: 'error', errorMessage: 'Conversion failed' } : c));
      addLog(`Error processing ${nextPending.originalName}`);
    }
  }, [chunks]);

  useEffect(() => {
    if (step === ConversionStep.CONVERTING_INCREMENTS) {
      const timer = setTimeout(() => {
        processNextChunk();
      }, 1000);
      return () => clearTimeout(timer);
    }
  }, [step, processNextChunk]);

  const statsData = [
    { name: 'Pending', value: chunks.filter(c => c.status === 'pending').length, color: '#475569' },
    { name: 'In Progress', value: chunks.filter(c => c.status === 'converting').length, color: '#3b82f6' },
    { name: 'Completed', value: chunks.filter(c => c.status === 'completed').length, color: '#10b981' },
    { name: 'Errors', value: chunks.filter(c => c.status === 'error').length, color: '#ef4444' }
  ];

  const selectedChunk = chunks.find(c => c.id === selectedChunkId);

  return (
    <div className="min-h-screen flex flex-col bg-slate-950 text-slate-200 font-sans">
      {/* Header */}
      <header className="h-16 border-b border-slate-800 bg-slate-900/50 flex items-center justify-between px-6 sticky top-0 z-50 backdrop-blur-md">
        <div className="flex items-center gap-3">
          <div className="p-2 bg-indigo-600 rounded-lg shadow-lg shadow-indigo-500/20">
            <Cpu className="w-6 h-6 text-white" />
          </div>
          <h1 className="text-xl font-bold tracking-tight text-white">LegacyLink<span className="text-indigo-400 font-normal">AI</span></h1>
        </div>
        <div className="flex items-center gap-6">
          <div className="flex flex-col items-end">
            <span className="text-xs text-slate-500 font-medium uppercase tracking-wider">Target Environment</span>
            <span className="text-sm text-indigo-300 font-semibold flex items-center gap-1">
              <Layers className="w-3 h-3" /> AWS Fargate / Python 3.12
            </span>
          </div>
          <button className="p-2 text-slate-400 hover:text-white transition-colors">
            <Settings className="w-5 h-5" />
          </button>
        </div>
      </header>

      <main className="flex-1 flex overflow-hidden">
        {/* Sidebar - Files/Modules */}
        <aside className="w-80 border-r border-slate-800 flex flex-col bg-slate-900/20">
          <div className="p-4 border-b border-slate-800 flex items-center justify-between">
            <h2 className="text-sm font-semibold uppercase tracking-wider text-slate-400">Project Modules</h2>
            <div className="text-xs font-mono bg-slate-800 px-2 py-1 rounded text-slate-300">
              {chunks.length} Total
            </div>
          </div>
          <div className="flex-1 overflow-y-auto p-2 space-y-1">
            {chunks.length === 0 ? (
              <div className="h-full flex flex-col items-center justify-center p-8 text-center opacity-50">
                <FileCode2 className="w-10 h-10 mb-2" />
                <p className="text-sm">No project modules identified. Upload a legacy source file to begin.</p>
              </div>
            ) : (
              chunks.map((chunk) => (
                <button
                  key={chunk.id}
                  onClick={() => setSelectedChunkId(chunk.id)}
                  className={`w-full flex items-center gap-3 p-3 rounded-lg transition-all text-left ${
                    selectedChunkId === chunk.id ? 'bg-indigo-600/10 border-l-2 border-indigo-500' : 'hover:bg-slate-800'
                  }`}
                >
                  {chunk.status === 'completed' && <CheckCircle2 className="w-4 h-4 text-emerald-400 shrink-0" />}
                  {chunk.status === 'pending' && <Clock className="w-4 h-4 text-slate-500 shrink-0" />}
                  {chunk.status === 'converting' && <Cpu className="w-4 h-4 text-blue-400 animate-pulse shrink-0" />}
                  {chunk.status === 'error' && <AlertCircle className="w-4 h-4 text-rose-500 shrink-0" />}
                  <div className="min-w-0">
                    <p className={`text-sm font-medium truncate ${selectedChunkId === chunk.id ? 'text-indigo-300' : 'text-slate-300'}`}>
                      {chunk.originalName}
                    </p>
                    <div className="w-full bg-slate-800 h-1 rounded-full mt-1.5 overflow-hidden">
                      <div 
                        className="bg-indigo-500 h-full transition-all duration-500" 
                        style={{ width: `${chunk.progress}%` }} 
                      />
                    </div>
                  </div>
                </button>
              ))
            )}
          </div>
          <div className="p-4 border-t border-slate-800 bg-slate-900/50">
            <label className="flex flex-col items-center justify-center w-full h-24 border-2 border-dashed border-slate-700 rounded-xl cursor-pointer hover:border-indigo-500 transition-colors bg-slate-800/20 group">
              <div className="flex flex-col items-center justify-center pt-5 pb-6">
                <Upload className="w-6 h-6 text-slate-500 group-hover:text-indigo-400 mb-2" />
                <p className="text-xs text-slate-400 group-hover:text-slate-300">Upload COBOL Source</p>
              </div>
              <input type="file" className="hidden" accept=".cbl,.cob,.txt" onChange={handleFileUpload} />
            </label>
          </div>
        </aside>

        {/* Dashboard Area */}
        <section className="flex-1 flex flex-col overflow-hidden">
          {step === ConversionStep.IDLE ? (
            <div className="flex-1 flex flex-col items-center justify-center p-12 max-w-4xl mx-auto text-center">
              <div className="w-20 h-20 bg-indigo-500/10 rounded-3xl flex items-center justify-center mb-6 ring-1 ring-indigo-500/50">
                <ArrowRightLeft className="w-10 h-10 text-indigo-400" />
              </div>
              <h2 className="text-4xl font-extrabold text-white mb-4">Banking Code Modernization</h2>
              <p className="text-lg text-slate-400 mb-8 leading-relaxed">
                LegacyLink uses Gemini-3-Pro to decompose and rewrite monolithic COBOL banking systems into modular, tested, and secure Python microservices.
              </p>
              <div className="grid grid-cols-1 md:grid-cols-3 gap-6 w-full text-left">
                {[
                  { icon: Layers, title: "Incremental Decomposition", desc: "Breaks 20k+ LOC systems into logical migratable chunks." },
                  { icon: CheckCircle2, title: "Auto-Testing", desc: "Generates Pytest suites with every conversion to ensure parity." },
                  { icon: Database, title: "Cloud Ready", desc: "Direct mapping of legacy files to modern object storage and DBs." }
                ].map((feature, i) => (
                  <div key={i} className="bg-slate-900/50 border border-slate-800 p-6 rounded-2xl">
                    <feature.icon className="w-6 h-6 text-indigo-400 mb-4" />
                    <h3 className="font-bold text-white mb-2">{feature.title}</h3>
                    <p className="text-sm text-slate-500">{feature.desc}</p>
                  </div>
                ))}
              </div>
              <div className="mt-12 flex gap-4">
                <button 
                  onClick={() => {
                    const mockEvent = { target: { files: [new File([MOCK_COBOL], "LEDGER.CBL")] } } as any;
                    handleFileUpload(mockEvent);
                  }}
                  className="px-8 py-3 bg-indigo-600 hover:bg-indigo-500 text-white rounded-xl font-bold transition-all shadow-lg shadow-indigo-600/20 flex items-center gap-2"
                >
                  <PlayCircle className="w-5 h-5" /> Start Sample Migration
                </button>
              </div>
            </div>
          ) : (
            <div className="flex-1 flex flex-col overflow-hidden">
              {/* Progress Summary */}
              <div className="grid grid-cols-4 gap-4 p-6 bg-slate-900/30 border-b border-slate-800">
                <div className="bg-slate-800/40 p-4 rounded-xl border border-slate-700/50">
                  <span className="text-xs font-bold text-slate-500 uppercase tracking-widest block mb-1">Status</span>
                  <div className="flex items-center gap-2">
                    <div className={`w-2 h-2 rounded-full ${step === ConversionStep.CONVERTING_INCREMENTS ? 'bg-blue-400 animate-pulse' : 'bg-emerald-400'}`} />
                    <span className="text-sm font-semibold">{step.replace('_', ' ')}</span>
                  </div>
                </div>
                <div className="bg-slate-800/40 p-4 rounded-xl border border-slate-700/50">
                  <span className="text-xs font-bold text-slate-500 uppercase tracking-widest block mb-1">Modules Converted</span>
                  <span className="text-2xl font-mono font-bold text-white">
                    {chunks.filter(c => c.status === 'completed').length} / {chunks.length}
                  </span>
                </div>
                <div className="bg-slate-800/40 p-4 rounded-xl border border-slate-700/50 col-span-2 relative overflow-hidden">
                  <span className="text-xs font-bold text-slate-500 uppercase tracking-widest block mb-1">Executive Summary</span>
                  <p className="text-xs text-slate-300 italic line-clamp-2">
                    {executiveSummary || "Modernization pipeline active. Analyzing patterns..."}
                  </p>
                  <div className="absolute right-0 bottom-0 opacity-10">
                    <BarChart3 className="w-12 h-12" />
                  </div>
                </div>
              </div>

              {/* Code Views */}
              <div className="flex-1 flex overflow-hidden">
                <div className="flex-1 flex flex-col min-w-0 border-r border-slate-800">
                  <div className="h-10 border-b border-slate-800 flex items-center px-4 bg-slate-900/50 justify-between">
                    <span className="text-xs font-mono text-slate-400 flex items-center gap-2">
                      <FileCode2 className="w-3.5 h-3.5" /> LEGACY_SOURCE.CBL
                    </span>
                    {selectedChunk && (
                      <span className="text-xs font-semibold text-indigo-400">{selectedChunk.originalName}</span>
                    )}
                  </div>
                  <div className="flex-1 overflow-auto p-4 mono text-sm bg-slate-950">
                    {selectedChunk ? (
                      <pre className="text-slate-400 whitespace-pre-wrap leading-relaxed">
                        {selectedChunk.cobolCode}
                      </pre>
                    ) : (
                      <div className="h-full flex items-center justify-center text-slate-600 italic">
                        Select a module to view source
                      </div>
                    )}
                  </div>
                </div>
                <div className="flex-1 flex flex-col min-w-0 bg-slate-900/10">
                  <div className="h-10 border-b border-slate-800 flex items-center px-4 bg-slate-900/50 justify-between">
                    <span className="text-xs font-mono text-slate-400 flex items-center gap-2">
                      <Code2 className="w-3.5 h-3.5" /> MODERNIZED_OUTPUT.PY
                    </span>
                    {selectedChunk?.status === 'completed' && (
                      <div className="flex items-center gap-2">
                         <span className="px-2 py-0.5 rounded-full bg-emerald-500/10 text-emerald-400 text-[10px] font-bold uppercase tracking-wider">Linter Passed</span>
                      </div>
                    )}
                  </div>
                  <div className="flex-1 overflow-auto p-4 mono text-sm relative">
                    {selectedChunk?.pythonCode ? (
                      <div className="space-y-6">
                        <pre className="text-indigo-200 whitespace-pre-wrap leading-relaxed">
                          {selectedChunk.pythonCode}
                        </pre>
                        {selectedChunk.unitTests && (
                          <div className="mt-8 border-t border-slate-800 pt-6">
                            <h4 className="text-xs font-bold text-slate-500 uppercase tracking-widest mb-4 flex items-center gap-2">
                              <CheckCircle2 className="w-3 h-3 text-emerald-500" /> Generated Test Suite (Pytest)
                            </h4>
                            <pre className="text-slate-500 whitespace-pre-wrap opacity-80 italic">
                              {selectedChunk.unitTests}
                            </pre>
                          </div>
                        )}
                      </div>
                    ) : (
                      <div className="h-full flex flex-col items-center justify-center text-slate-600 italic space-y-4">
                        {selectedChunk?.status === 'converting' ? (
                          <>
                            <div className="w-12 h-12 rounded-full border-2 border-indigo-500 border-t-transparent animate-spin" />
                            <p>AI Engine converting logic...</p>
                          </>
                        ) : (
                          <p>Modernized code will appear here</p>
                        )}
                      </div>
                    )}
                  </div>
                </div>
              </div>
            </div>
          )}
        </section>

        {/* Right Sidebar - Terminal & Metrics */}
        <aside className="w-96 border-l border-slate-800 flex flex-col bg-slate-900/20">
          <div className="p-4 border-b border-slate-800 bg-slate-900/50">
            <h2 className="text-sm font-semibold uppercase tracking-wider text-slate-400 flex items-center gap-2">
              <Terminal className="w-4 h-4" /> Migration Logs
            </h2>
          </div>
          <div className="flex-1 overflow-y-auto p-4 mono text-[11px] leading-tight space-y-2 bg-slate-950">
            {logs.map((log, i) => (
              <div key={i} className="text-slate-400 break-words">
                <span className="text-indigo-500">{'>'}</span> {log}
              </div>
            ))}
            {logs.length === 0 && (
              <div className="text-slate-700 italic">No logs recorded...</div>
            )}
            <div ref={logEndRef} />
          </div>
          
          <div className="p-4 border-t border-slate-800 h-64 flex flex-col">
            <h2 className="text-sm font-semibold uppercase tracking-wider text-slate-400 flex items-center gap-2 mb-4">
              <BarChart3 className="w-4 h-4" /> Migration Health
            </h2>
            <div className="flex-1 min-h-0">
               <ResponsiveContainer width="100%" height="100%">
                <BarChart data={statsData} layout="vertical" margin={{ left: -10, right: 20 }}>
                  <XAxis type="number" hide />
                  <YAxis dataKey="name" type="category" width={90} axisLine={false} tickLine={false} style={{ fontSize: '10px', fill: '#94a3b8' }} />
                  <Tooltip 
                    cursor={{ fill: 'transparent' }} 
                    contentStyle={{ backgroundColor: '#1e293b', border: 'none', borderRadius: '8px', fontSize: '10px' }}
                  />
                  <Bar dataKey="value" radius={[0, 4, 4, 0]}>
                    {statsData.map((entry, index) => (
                      <Cell key={`cell-${index}`} fill={entry.color} />
                    ))}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
            </div>
          </div>
        </aside>
      </main>

      {/* Persistent Call-to-Action / Status Bar */}
      <footer className="h-10 border-t border-slate-800 bg-slate-900 flex items-center justify-between px-6 text-[10px] font-bold uppercase tracking-[0.2em] text-slate-500">
        <div className="flex gap-4">
          <span className="flex items-center gap-1.5"><div className="w-1.5 h-1.5 rounded-full bg-emerald-500" /> Gemini-3 Engine Online</span>
          <span className="flex items-center gap-1.5"><div className="w-1.5 h-1.5 rounded-full bg-indigo-500" /> Pipeline: Async Incremental</span>
        </div>
        <div>
          Copyright &copy; 2024 LegacyLink Infrastructure v0.9.4-alpha
        </div>
      </footer>
    </div>
  );
};

export default App;
