type Array<T> = { [number]: T }

local SyntaxNode = {}

local NodeObject = {}
NodeObject.__index = NodeObject

export type NodePositionInfo = {
	Index: number,
	Line: number,
	Column: number
}

export type NodePosition = {
	Start: NodePositionInfo,
	End: NodePositionInfo
}

export type SyntaxNode = typeof(setmetatable(
	{} :: {
		Kind: SyntaxNodeKind,
		Position: NodePosition
	},
	NodeObject
))
export type SyntaxNodeKind = number
export type BinaryOperationKind = number
export type UnaryOperationKind = number
export type CompoundOperationKind = number

function NodeObject.is(self: SyntaxNode, kind: SyntaxNodeKind): boolean
	return self.Kind == kind
end

local DummyNodePositionInfo: NodePositionInfo = {
	Index = -1,
	Line = -1,
	Column = -1
}
local DummyPosition: NodePosition = {
	Start = DummyNodePositionInfo,
	End = DummyNodePositionInfo
}

SyntaxNode.BinaryOperationKind = {
	Add = 1,
	Subtract = 2,
	Multiply = 3,
	Divide = 4,
	Modulo = 5,
	Power = 6,
	CompareNotEqual = 7,
	CompareEqual = 8,
	CompareLessThan = 9,
	CompareLessEqual = 10,
	CompareGreaterThan = 11,
	CompareGreaterEqual = 12,
	And = 13,
	Or = 14,
	Concaternate = 15
}

SyntaxNode.UnaryOperationKind = {
	Length = 1,
	Not = 2,
	Negate = 3
}

SyntaxNode.CompoundOperationKind = {
	Add = 1,
	Subtract = 2,
	Multiply = 3,
	Divide = 4,
	Modulo = 5,
	Power = 6,
	Concaternate = 7
}

SyntaxNode.SyntaxNodeKind = { -- TODO reorder this
	Error = 0,
	ErrorStatement = 1,

	Return = 2,
	Break = 3,
	Continue = 4,

	Scope = 5,

	Identifier = 6,

	AssignmentStatement = 7,
	Global = 8,
	Local = 9,
	GlobalReassignment = 10,
	LocalReassignment = 11,

	IndexIdentifier = 12,
	MethodIdentifier = 13,

	FunctionCall = 14,
	FunctionDefinition = 15,

	Table = 16,
	TableField = 17,

	BinaryOperation = 18,
	UnaryOperation = 19,
	CompoundOperation = 20,

	True = 30,
	False = 31,
	Nil = 32,
	Dot3 = 33,

	String = 34,
	Number = 35,

	IfStatement = 38,
	IfElseExpression = 39,

	LocalFunction = 41,
	Function = 42,

	ExpressionList = 44,

	BindingList = 45,
	Binding = 46,

	Do = 47,

	WhileLoop = 50,
	RepeatLoop = 51,

	InterpolatedString = 53,

	NumericalLoop = 54,
	GenericLoop = 55
}

-- Most generic function. No value, whatsoever.
function SyntaxNode.MakeNode(Kind: SyntaxNodeKind, Position: NodePosition): SyntaxNode
	return setmetatable({
		Kind = Kind,
		Position = Position
	}, NodeObject)
end

-- MakeNode but with value
function SyntaxNode.MakeValueNode(Kind: SyntaxNodeKind, Value: any, Position: NodePosition): ValueSyntaxNode
	return setmetatable({
		Kind = Kind,
		Position = Position,
		Value = Value
	}, NodeObject)
end
export type ValueSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeValueNode(-1, nil, DummyPosition))

function SyntaxNode.MakeBinaryOperation(OperationKind: BinaryOperationKind, LeftExpression: SyntaxNode, RightExpression: SyntaxNode, Position: NodePosition): BinaryOperationSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.BinaryOperation,
		Position = Position,
		OperationKind = OperationKind,
		LeftExpression = LeftExpression,
		RightExpression = RightExpression
	}, NodeObject)
end
export type BinaryOperationSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeBinaryOperation(-1, nil :: any, nil :: any, DummyPosition))

function SyntaxNode.MakeUnaryOperation(OperationKind: UnaryOperationKind, Expression: SyntaxNode, Position: NodePosition): UnaryOperationSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.UnaryOperation,
		Position = Position,
		OperationKind = OperationKind,
		Expression = Expression,
	}, NodeObject)
end
export type UnaryOperationSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeUnaryOperation(-1, nil :: any, DummyPosition))

function SyntaxNode.MakeCompoundOperation(OperationKind: CompoundOperationKind, Variable: SyntaxNode, Value: SyntaxNode, Position: NodePosition): CompoundOperationSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.CompoundOperation,
		Position = Position,
		OperationKind = OperationKind,
		Variable = Variable,
		Value = Value
	}, NodeObject)
end
export type CompoundOperationSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeCompoundOperation(-1, nil :: any, nil :: any, DummyPosition))

-- Node-specific create functions
function SyntaxNode.MakeError(ErrorMessage: string, IsStatement: boolean, Position: NodePosition): ErrorSyntaxNode
	return setmetatable({
		Kind = IsStatement and SyntaxNode.SyntaxNodeKind.ErrorStatement or SyntaxNode.SyntaxNodeKind.Error,
		Position = Position,
		Error = ErrorMessage
	}, NodeObject)
end
export type ErrorSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeError("", false, DummyPosition))

function SyntaxNode.MakeScope(Statements: Array<SyntaxNode>, Position: NodePosition): ScopeSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.Scope,
		Position = Position,
		Statements = Statements
	}, NodeObject)
end
export type ScopeSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeScope({}, DummyPosition))

function SyntaxNode.MakeBinding(Identifier: ValueSyntaxNode, Position: NodePosition): BindingSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.Binding,
		Position = Position,
		Identifier = Identifier
	}, NodeObject)
end
export type BindingSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeBinding(nil :: any, DummyPosition))

function SyntaxNode.MakeAssignment(Variable: SyntaxNode, Value: SyntaxNode?, IsLocal: boolean, IsReassignment: boolean, Position: NodePosition): AssignmentSyntaxNode
	local Kind = SyntaxNode.SyntaxNodeKind.Global
	if IsLocal then
		Kind = SyntaxNode.SyntaxNodeKind.Local
		if IsReassignment then
			Kind = SyntaxNode.SyntaxNodeKind.LocalReassignment
		end
	end
	if IsReassignment then
		Kind = SyntaxNode.SyntaxNodeKind.GlobalReassignment
	end

	return setmetatable({
		Kind = Kind,
		Position = Position,
		Variable = Variable,
		Value = Value
	}, NodeObject)
end
export type AssignmentSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeAssignment(nil :: any, nil :: any, false, false, DummyPosition))

function SyntaxNode.MakeAssignmentStatement(Assignments: Array<AssignmentSyntaxNode>, Position: NodePosition): AssignmentStatementSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.AssignmentStatement,
		Position = Position,
		Assignments = Assignments
	}, NodeObject)
end
export type AssignmentStatementSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeAssignmentStatement({}, DummyPosition))

function SyntaxNode.MakeIndexIdentifier(Identifiers: Array<ValueSyntaxNode>, Position: NodePosition): IndexIdentifierSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.IndexIdentifier,
		Position = Position,
		Identifiers = Identifiers
	}, NodeObject)
end
export type IndexIdentifierSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeIndexIdentifier({}, DummyPosition))

function SyntaxNode.MakeMethodIdentifier(IndexIdentifier: IndexIdentifierSyntaxNode, MethodName: ValueSyntaxNode, Position: NodePosition): MethodIdentifierSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.MethodIdentifier,
		Position = Position,
		IndexIdentifier = IndexIdentifier,
		MethodName = MethodName
	}, NodeObject)
end
export type MethodIdentifierSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeMethodIdentifier(nil :: any, nil :: any, DummyPosition))

function SyntaxNode.MakeFunctionCall(Identifier: ValueSyntaxNode | IndexIdentifierSyntaxNode | MethodIdentifierSyntaxNode, Arguments: Array<SyntaxNode>, Position: NodePosition)
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.FunctionCall,
		Position = Position,
		Identifier = Identifier,
		Arguments = Arguments
	}, NodeObject)
end
export type FunctionCallSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeFunctionCall(nil :: any, {}, DummyPosition))

function SyntaxNode.MakeTable(Fields: Array<TableFieldSyntaxNode>, Position: NodePosition): TableSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.TableField,
		Position = Position,
		Fields = Fields
	}, NodeObject)
end
export type TableSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeTable({}, DummyPosition))

function SyntaxNode.MakeTableField(Key: SyntaxNode, Value: SyntaxNode?, Position: NodePosition): TableFieldSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.TableField,
		Position = Position,
		Key = Key,
		Value = Value
	}, NodeObject)
end
export type TableFieldSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeTableField(nil :: any, nil, DummyPosition))

function SyntaxNode.MakeIfElse(Condition: SyntaxNode, ThenExpression: SyntaxNode, ElseExpression: SyntaxNode?, ElseifExpression: IfElseExpressionNode?, Position: NodePosition): IfElseExpressionNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.IfElseExpression,
		Position = Position,
		Condition = Condition,
		ThenExpression = ThenExpression,
		ElseExpression = ElseExpression,
		ElseifExpression = ElseifExpression,
	}, NodeObject)
end
export type IfElseExpressionNode = SyntaxNode & typeof(SyntaxNode.MakeIfElse(nil :: any, nil :: any, nil, nil, DummyPosition))

function SyntaxNode.MakeIf(Condition: SyntaxNode, ThenExpression: ScopeSyntaxNode, ElseExpression: SyntaxNode?, ElseifExpression: IfNode?, Position: NodePosition): IfNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.IfStatement,
		Position = Position,
		Condition = Condition,
		Expression = ThenExpression,
		ElseExpression = ElseExpression,
		ElseifExpression = ElseifExpression,
	}, NodeObject)
end
export type IfNode = SyntaxNode & typeof(SyntaxNode.MakeIf(nil :: any, nil :: any, nil, nil, DummyPosition))

function SyntaxNode.MakeFunctionDefinition(Parameters: Array<BindingSyntaxNode>, Scope: ScopeSyntaxNode, Position: NodePosition): FunctionDefinitionSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.FunctionDefinition,
		Position = Position,
		Parameters = Parameters,
		Scope = Scope
	}, NodeObject)
end
export type FunctionDefinitionSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeFunctionDefinition(nil :: any, nil :: any, DummyPosition))

function SyntaxNode.MakeFunction(FunctionName: ValueSyntaxNode | IndexIdentifierSyntaxNode | MethodIdentifierSyntaxNode, Definition: FunctionDefinitionSyntaxNode, IsLocal: boolean, Position: NodePosition): FunctionSyntaxNode
	return setmetatable({
		Kind = IsLocal and SyntaxNode.SyntaxNodeKind.LocalFunction or SyntaxNode.SyntaxNodeKind.Function,
		Position = Position,
		Name = FunctionName,
		Definition = Definition
	}, NodeObject)
end
export type FunctionSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeFunction(nil :: any, nil :: any, false, DummyPosition))

function SyntaxNode.MakeExpressionList(Expressions: Array<SyntaxNode>, Position: NodePosition): ExpressionListSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.ExpressionList,
		Position = Position,
		Expressions = Expressions
	}, NodeObject)
end
export type ExpressionListSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeExpressionList(nil :: any, DummyPosition))

function SyntaxNode.MakeDo(Scope: ScopeSyntaxNode, Position: NodePosition): DoSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.Do,
		Position = Position,
		Scope = Scope
	}, NodeObject)
end
export type DoSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeDo(nil :: any, DummyPosition))

function SyntaxNode.MakeWhile(Condition: SyntaxNode, Scope: ScopeSyntaxNode, Position: NodePosition): WhileSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.WhileLoop,
		Position = Position,
		Condition = Condition,
		Scope = Scope
	}, NodeObject)
end
export type WhileSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeWhile(nil :: any, nil :: any, DummyPosition))

function SyntaxNode.MakeRepeatUntil(UntilCondition: SyntaxNode, Scope: ScopeSyntaxNode, Position: NodePosition): RepeatUntilSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.RepeatLoop,
		Position = Position,
		UntilCondition = UntilCondition,
		Scope = Scope
	}, NodeObject)
end
export type RepeatUntilSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeRepeatUntil(nil :: any, nil :: any, DummyPosition))

function SyntaxNode.MakeInterpolatedString(Expressions: Array<SyntaxNode>, Position: NodePosition): InterpolatedStringSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.InterpolatedString,
		Position = Position,
		Expressions =  Expressions
	}, NodeObject)
end
export type InterpolatedStringSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeInterpolatedString({}, DummyPosition))

function SyntaxNode.MakeNumericalFor(Binding: BindingSyntaxNode, Start: SyntaxNode, End: SyntaxNode, Step: SyntaxNode?, Scope: ScopeSyntaxNode, Position: NodePosition): NumericalForSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.NumericalLoop,
		Position = Position,
		Binding = Binding,
		Start = Start,
		End = End,
		Step = Step,
		Scope = Scope
	}, NodeObject)
end
export type NumericalForSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeNumericalFor(nil :: any, nil :: any, nil :: any, nil, nil :: any, DummyPosition))

function SyntaxNode.MakeGenericFor(Variables: Array<BindingSyntaxNode>, Values: Array<SyntaxNode>, Scope: ScopeSyntaxNode, Position: NodePosition): GenericForSyntaxNode
	return setmetatable({
		Kind = SyntaxNode.SyntaxNodeKind.GenericLoop,
		Position = Position,
		Variables = Variables,
		Values = Values,
		Scope = Scope
	}, NodeObject)
end
export type GenericForSyntaxNode = SyntaxNode & typeof(SyntaxNode.MakeGenericFor({}, {}, nil :: any, DummyPosition))

return SyntaxNode
