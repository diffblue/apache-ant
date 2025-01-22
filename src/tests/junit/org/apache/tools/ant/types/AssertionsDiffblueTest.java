package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.List;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Assertions.BaseAssertion;
import org.apache.tools.ant.types.Assertions.DisabledAssertion;
import org.apache.tools.ant.types.Assertions.EnabledAssertion;
import org.apache.tools.ant.types.selectors.modifiedselector.ModifiedSelector;
import org.junit.Test;

public class AssertionsDiffblueTest {
  /**
   * Test {@link Assertions#addEnable(EnabledAssertion)}.
   * <p>
   * Method under test: {@link Assertions#addEnable(EnabledAssertion)}
   */
  @Test
  public void testAddEnable() {
    // Arrange
    Assertions assertions = new Assertions();

    // Act
    assertions.addEnable(new EnabledAssertion());

    // Assert
    assertEquals(1, assertions.size());
  }

  /**
   * Test {@link Assertions#addDisable(DisabledAssertion)}.
   * <p>
   * Method under test: {@link Assertions#addDisable(DisabledAssertion)}
   */
  @Test
  public void testAddDisable() {
    // Arrange
    Assertions assertions = new Assertions();

    // Act
    assertions.addDisable(new DisabledAssertion());

    // Assert
    assertEquals(1, assertions.size());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#getClassName()}.
   * <p>
   * Method under test: {@link BaseAssertion#getClassName()}
   */
  @Test
  public void testBaseAssertionGetClassName() {
    // Arrange, Act and Assert
    assertNull((new DisabledAssertion()).getClassName());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#getPackageName()}.
   * <p>
   * Method under test: {@link BaseAssertion#getPackageName()}
   */
  @Test
  public void testBaseAssertionGetPackageName() {
    // Arrange, Act and Assert
    assertNull((new DisabledAssertion()).getPackageName());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#setClass(String)}.
   * <p>
   * Method under test: {@link BaseAssertion#setClass(String)}
   */
  @Test
  public void testBaseAssertionSetClass() {
    // Arrange
    DisabledAssertion disabledAssertion = new DisabledAssertion();

    // Act
    disabledAssertion.setClass("Class Name");

    // Assert
    assertEquals("Class Name", disabledAssertion.getClassName());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#setPackage(String)}.
   * <p>
   * Method under test: {@link BaseAssertion#setPackage(String)}
   */
  @Test
  public void testBaseAssertionSetPackage() {
    // Arrange
    DisabledAssertion disabledAssertion = new DisabledAssertion();

    // Act
    disabledAssertion.setPackage("java.text");

    // Assert
    assertEquals("java.text", disabledAssertion.getPackageName());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#toCommand()}.
   * <ul>
   *   <li>Given {@link DisabledAssertion} (default constructor) Package is {@code ...}.</li>
   *   <li>Then return {@code -da:...}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseAssertion#toCommand()}
   */
  @Test
  public void testBaseAssertionToCommand_givenDisabledAssertionPackageIsDotDotDot_thenReturnDa() {
    // Arrange
    DisabledAssertion disabledAssertion = new DisabledAssertion();
    disabledAssertion.setPackage("...");
    disabledAssertion.setClass(null);

    // Act and Assert
    assertEquals("-da:...", disabledAssertion.toCommand());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#toCommand()}.
   * <ul>
   *   <li>Given {@link DisabledAssertion} (default constructor) Package is {@code foo}.</li>
   *   <li>Then return {@code -da:foo...}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseAssertion#toCommand()}
   */
  @Test
  public void testBaseAssertionToCommand_givenDisabledAssertionPackageIsFoo_thenReturnDaFoo() {
    // Arrange
    DisabledAssertion disabledAssertion = new DisabledAssertion();
    disabledAssertion.setPackage("foo");
    disabledAssertion.setClass(null);

    // Act and Assert
    assertEquals("-da:foo...", disabledAssertion.toCommand());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#toCommand()}.
   * <ul>
   *   <li>Given {@link DisabledAssertion} (default constructor) Package is {@code null}.</li>
   *   <li>Then return {@code -da:foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseAssertion#toCommand()}
   */
  @Test
  public void testBaseAssertionToCommand_givenDisabledAssertionPackageIsNull_thenReturnDaFoo() {
    // Arrange
    DisabledAssertion disabledAssertion = new DisabledAssertion();
    disabledAssertion.setPackage(null);
    disabledAssertion.setClass("foo");

    // Act and Assert
    assertEquals("-da:foo", disabledAssertion.toCommand());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#toCommand()}.
   * <ul>
   *   <li>Given {@link DisabledAssertion} (default constructor).</li>
   *   <li>Then return {@code -da}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseAssertion#toCommand()}
   */
  @Test
  public void testBaseAssertionToCommand_givenDisabledAssertion_thenReturnDa() {
    // Arrange, Act and Assert
    assertEquals("-da", (new DisabledAssertion()).toCommand());
  }

  /**
   * Test BaseAssertion {@link BaseAssertion#toCommand()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link BaseAssertion#toCommand()}
   */
  @Test
  public void testBaseAssertionToCommand_thenThrowBuildException() {
    // Arrange
    DisabledAssertion disabledAssertion = new DisabledAssertion();
    disabledAssertion.setPackage("foo");
    disabledAssertion.setClass("foo");

    // Act and Assert
    assertThrows(BuildException.class, () -> disabledAssertion.toCommand());
  }

  /**
   * Test DisabledAssertion getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link DisabledAssertion}
   *   <li>{@link DisabledAssertion#getCommandPrefix()}
   * </ul>
   */
  @Test
  public void testDisabledAssertionGettersAndSetters() {
    // Arrange and Act
    DisabledAssertion actualDisabledAssertion = new DisabledAssertion();

    // Assert
    assertEquals("-da", actualDisabledAssertion.getCommandPrefix());
    assertNull(actualDisabledAssertion.getClassName());
    assertNull(actualDisabledAssertion.getPackageName());
  }

  /**
   * Test EnabledAssertion getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link EnabledAssertion}
   *   <li>{@link EnabledAssertion#getCommandPrefix()}
   * </ul>
   */
  @Test
  public void testEnabledAssertionGettersAndSetters() {
    // Arrange and Act
    EnabledAssertion actualEnabledAssertion = new EnabledAssertion();

    // Assert
    assertEquals("-ea", actualEnabledAssertion.getCommandPrefix());
    assertNull(actualEnabledAssertion.getClassName());
    assertNull(actualEnabledAssertion.getPackageName());
  }

  /**
   * Test {@link Assertions#setEnableSystemAssertions(Boolean)}.
   * <p>
   * Method under test: {@link Assertions#setEnableSystemAssertions(Boolean)}
   */
  @Test
  public void testSetEnableSystemAssertions() {
    // Arrange
    Assertions assertions = new Assertions();

    // Act
    assertions.setEnableSystemAssertions(true);

    // Assert
    assertEquals(1, assertions.size());
  }

  /**
   * Test {@link Assertions#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link Assertions} (default constructor).</li>
   *   <li>Then not {@link Assertions} (default constructor) Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenAssertions_thenNotAssertionsChecked() {
    // Arrange
    Assertions assertions = new Assertions();
    Reference ref = new Reference("42");

    // Act
    assertions.setRefid(ref);

    // Assert
    assertFalse(assertions.isChecked());
    assertTrue(assertions.isReference());
    assertSame(ref, assertions.getRefid());
  }

  /**
   * Test {@link Assertions#size()}.
   * <ul>
   *   <li>Given {@link Assertions} (default constructor) EnableSystemAssertions is {@code true}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#size()}
   */
  @Test
  public void testSize_givenAssertionsEnableSystemAssertionsIsTrue_thenReturnOne() {
    // Arrange
    Assertions assertions = new Assertions();
    assertions.setEnableSystemAssertions(true);

    // Act and Assert
    assertEquals(1, assertions.size());
  }

  /**
   * Test {@link Assertions#size()}.
   * <ul>
   *   <li>Given {@link Assertions} (default constructor).</li>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#size()}
   */
  @Test
  public void testSize_givenAssertions_thenReturnZero() {
    // Arrange, Act and Assert
    assertEquals(0, (new Assertions()).size());
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then {@link ArrayList#ArrayList()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_givenJavaLangObject_thenArrayListEmpty() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Type Name", typeClass);
    project.addBuildListener(new AntClassLoader());

    Assertions assertions = new Assertions();
    assertions.setProject(project);
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert that nothing has changed
    assertTrue(commandList.isEmpty());
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    Assertions assertions = new Assertions();
    assertions.setProject(project);
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert that nothing has changed
    assertTrue(commandList.isEmpty());
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link ModifiedSelector} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_givenProjectAddBuildListenerModifiedSelector() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new ModifiedSelector());

    Assertions assertions = new Assertions();
    assertions.setProject(project);
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert that nothing has changed
    assertTrue(commandList.isEmpty());
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Then {@link ArrayList#ArrayList()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_thenArrayListEmpty() {
    // Arrange
    Assertions assertions = new Assertions();
    assertions.setProject(new Project());
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert that nothing has changed
    assertTrue(commandList.isEmpty());
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Then {@link ArrayList#ArrayList()} Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_thenArrayListEmpty2() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Assertions assertions = new Assertions();
    assertions.setProject(project);
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert that nothing has changed
    assertTrue(commandList.isEmpty());
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Then {@link ArrayList#ArrayList()} first is {@code -disablesystemassertions}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_thenArrayListFirstIsDisablesystemassertions() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Assertions assertions = new Assertions();
    assertions.setEnableSystemAssertions(false);
    assertions.setProject(project);
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert
    assertEquals(1, commandList.size());
    assertEquals("-disablesystemassertions", commandList.get(0));
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Then {@link ArrayList#ArrayList()} first is {@code -ea}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_thenArrayListFirstIsEa() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Assertions assertions = new Assertions();
    assertions.addEnable(new EnabledAssertion());
    assertions.setProject(project);
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert
    assertEquals(1, commandList.size());
    assertEquals("-ea", commandList.get(0));
  }

  /**
   * Test {@link Assertions#applyAssertions(List)} with {@code commandList}.
   * <ul>
   *   <li>Then {@link ArrayList#ArrayList()} first is {@code -enablesystemassertions}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(List)}
   */
  @Test
  public void testApplyAssertionsWithCommandList_thenArrayListFirstIsEnablesystemassertions() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Assertions assertions = new Assertions();
    assertions.setEnableSystemAssertions(true);
    assertions.setProject(project);
    ArrayList<String> commandList = new ArrayList<>();

    // Act
    assertions.applyAssertions(commandList);

    // Assert
    assertEquals(1, commandList.size());
    assertEquals("-enablesystemassertions", commandList.get(0));
  }

  /**
   * Test {@link Assertions#applyAssertions(CommandlineJava)} with {@code command}.
   * <ul>
   *   <li>Given {@link Assertions} (default constructor).</li>
   *   <li>Then array length is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(CommandlineJava)}
   */
  @Test
  public void testApplyAssertionsWithCommand_givenAssertions_thenArrayLengthIsZero() {
    // Arrange
    Assertions assertions = new Assertions();
    CommandlineJava command = new CommandlineJava();

    // Act
    assertions.applyAssertions(command);

    // Assert that nothing has changed
    Commandline actualVMCommand = command.getActualVMCommand();
    assertEquals(0, actualVMCommand.getArguments().length);
    Commandline vmCommand = command.getVmCommand();
    assertEquals(0, vmCommand.getArguments().length);
    assertEquals(1, actualVMCommand.getCommandline().length);
    assertEquals(1, vmCommand.getCommandline().length);
    assertEquals(1, command.getCommandline().length);
  }

  /**
   * Test {@link Assertions#applyAssertions(CommandlineJava)} with {@code command}.
   * <ul>
   *   <li>Then second element is {@code -disablesystemassertions}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(CommandlineJava)}
   */
  @Test
  public void testApplyAssertionsWithCommand_thenSecondElementIsDisablesystemassertions() {
    // Arrange
    Assertions assertions = new Assertions();
    assertions.setEnableSystemAssertions(false);
    CommandlineJava command = new CommandlineJava();

    // Act
    assertions.applyAssertions(command);

    // Assert
    Commandline actualVMCommand = command.getActualVMCommand();
    String[] commandline = actualVMCommand.getCommandline();
    assertEquals("-disablesystemassertions", commandline[1]);
    Commandline vmCommand = command.getVmCommand();
    String[] commandline2 = vmCommand.getCommandline();
    assertEquals("-disablesystemassertions", commandline2[1]);
    String[] commandline3 = command.getCommandline();
    assertEquals("-disablesystemassertions", commandline3[1]);
    assertEquals(2, commandline.length);
    assertEquals(2, commandline2.length);
    assertEquals(2, commandline3.length);
    assertArrayEquals(new String[]{"-disablesystemassertions"}, actualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-disablesystemassertions"}, vmCommand.getArguments());
  }

  /**
   * Test {@link Assertions#applyAssertions(CommandlineJava)} with {@code command}.
   * <ul>
   *   <li>Then second element is {@code -ea}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(CommandlineJava)}
   */
  @Test
  public void testApplyAssertionsWithCommand_thenSecondElementIsEa() {
    // Arrange
    Assertions assertions = new Assertions();
    assertions.addEnable(new EnabledAssertion());
    CommandlineJava command = new CommandlineJava();

    // Act
    assertions.applyAssertions(command);

    // Assert
    Commandline actualVMCommand = command.getActualVMCommand();
    String[] commandline = actualVMCommand.getCommandline();
    assertEquals("-ea", commandline[1]);
    Commandline vmCommand = command.getVmCommand();
    String[] commandline2 = vmCommand.getCommandline();
    assertEquals("-ea", commandline2[1]);
    String[] commandline3 = command.getCommandline();
    assertEquals("-ea", commandline3[1]);
    assertEquals(2, commandline.length);
    assertEquals(2, commandline2.length);
    assertEquals(2, commandline3.length);
    assertArrayEquals(new String[]{"-ea"}, actualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-ea"}, vmCommand.getArguments());
  }

  /**
   * Test {@link Assertions#applyAssertions(CommandlineJava)} with {@code command}.
   * <ul>
   *   <li>Then second element is {@code -ea:...}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(CommandlineJava)}
   */
  @Test
  public void testApplyAssertionsWithCommand_thenSecondElementIsEa2() {
    // Arrange
    EnabledAssertion assertion = new EnabledAssertion();
    assertion.setPackage("...");

    Assertions assertions = new Assertions();
    assertions.addEnable(assertion);
    CommandlineJava command = new CommandlineJava();

    // Act
    assertions.applyAssertions(command);

    // Assert
    Commandline actualVMCommand = command.getActualVMCommand();
    String[] commandline = actualVMCommand.getCommandline();
    assertEquals("-ea:...", commandline[1]);
    Commandline vmCommand = command.getVmCommand();
    String[] commandline2 = vmCommand.getCommandline();
    assertEquals("-ea:...", commandline2[1]);
    String[] commandline3 = command.getCommandline();
    assertEquals("-ea:...", commandline3[1]);
    assertEquals(2, commandline.length);
    assertEquals(2, commandline2.length);
    assertEquals(2, commandline3.length);
    assertArrayEquals(new String[]{"-ea:..."}, actualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-ea:..."}, vmCommand.getArguments());
  }

  /**
   * Test {@link Assertions#applyAssertions(CommandlineJava)} with {@code command}.
   * <ul>
   *   <li>Then second element is {@code -ea:-ea}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(CommandlineJava)}
   */
  @Test
  public void testApplyAssertionsWithCommand_thenSecondElementIsEaEa() {
    // Arrange
    EnabledAssertion assertion = new EnabledAssertion();
    assertion.setClass("-ea");

    Assertions assertions = new Assertions();
    assertions.addEnable(assertion);
    CommandlineJava command = new CommandlineJava();

    // Act
    assertions.applyAssertions(command);

    // Assert
    Commandline actualVMCommand = command.getActualVMCommand();
    String[] commandline = actualVMCommand.getCommandline();
    assertEquals("-ea:-ea", commandline[1]);
    Commandline vmCommand = command.getVmCommand();
    String[] commandline2 = vmCommand.getCommandline();
    assertEquals("-ea:-ea", commandline2[1]);
    String[] commandline3 = command.getCommandline();
    assertEquals("-ea:-ea", commandline3[1]);
    assertEquals(2, commandline.length);
    assertEquals(2, commandline2.length);
    assertEquals(2, commandline3.length);
    assertArrayEquals(new String[]{"-ea:-ea"}, actualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-ea:-ea"}, vmCommand.getArguments());
  }

  /**
   * Test {@link Assertions#applyAssertions(CommandlineJava)} with {@code command}.
   * <ul>
   *   <li>Then second element is {@code -ea:java.text...}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(CommandlineJava)}
   */
  @Test
  public void testApplyAssertionsWithCommand_thenSecondElementIsEaJavaText() {
    // Arrange
    EnabledAssertion assertion = new EnabledAssertion();
    assertion.setPackage("java.text");

    Assertions assertions = new Assertions();
    assertions.addEnable(assertion);
    CommandlineJava command = new CommandlineJava();

    // Act
    assertions.applyAssertions(command);

    // Assert
    Commandline actualVMCommand = command.getActualVMCommand();
    String[] commandline = actualVMCommand.getCommandline();
    assertEquals("-ea:java.text...", commandline[1]);
    Commandline vmCommand = command.getVmCommand();
    String[] commandline2 = vmCommand.getCommandline();
    assertEquals("-ea:java.text...", commandline2[1]);
    String[] commandline3 = command.getCommandline();
    assertEquals("-ea:java.text...", commandline3[1]);
    assertEquals(2, commandline.length);
    assertEquals(2, commandline2.length);
    assertEquals(2, commandline3.length);
    assertArrayEquals(new String[]{"-ea:java.text..."}, actualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-ea:java.text..."}, vmCommand.getArguments());
  }

  /**
   * Test {@link Assertions#applyAssertions(CommandlineJava)} with {@code command}.
   * <ul>
   *   <li>Then second element is {@code -enablesystemassertions}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#applyAssertions(CommandlineJava)}
   */
  @Test
  public void testApplyAssertionsWithCommand_thenSecondElementIsEnablesystemassertions() {
    // Arrange
    Assertions assertions = new Assertions();
    assertions.setEnableSystemAssertions(true);
    CommandlineJava command = new CommandlineJava();

    // Act
    assertions.applyAssertions(command);

    // Assert
    Commandline actualVMCommand = command.getActualVMCommand();
    String[] commandline = actualVMCommand.getCommandline();
    assertEquals("-enablesystemassertions", commandline[1]);
    Commandline vmCommand = command.getVmCommand();
    String[] commandline2 = vmCommand.getCommandline();
    assertEquals("-enablesystemassertions", commandline2[1]);
    String[] commandline3 = command.getCommandline();
    assertEquals("-enablesystemassertions", commandline3[1]);
    assertEquals(2, commandline.length);
    assertEquals(2, commandline2.length);
    assertEquals(2, commandline3.length);
    assertArrayEquals(new String[]{"-enablesystemassertions"}, actualVMCommand.getArguments());
    assertArrayEquals(new String[]{"-enablesystemassertions"}, vmCommand.getArguments());
  }

  /**
   * Test {@link Assertions#clone()}.
   * <ul>
   *   <li>Given {@link Assertions} (default constructor) Checked is {@code false}.</li>
   *   <li>Then return not Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#clone()}
   */
  @Test
  public void testClone_givenAssertionsCheckedIsFalse_thenReturnNotChecked() throws CloneNotSupportedException {
    // Arrange
    Assertions assertions = new Assertions();
    assertions.setChecked(false);

    // Act
    Object actualCloneResult = assertions.clone();

    // Assert
    assertTrue(actualCloneResult instanceof Assertions);
    assertEquals("Assertions", ((Assertions) actualCloneResult).getDataTypeName());
    Location location = ((Assertions) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Assertions) actualCloneResult).getDescription());
    assertNull(((Assertions) actualCloneResult).getProject());
    assertNull(((Assertions) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, ((Assertions) actualCloneResult).size());
    assertFalse(((Assertions) actualCloneResult).isChecked());
    assertFalse(((Assertions) actualCloneResult).isReference());
  }

  /**
   * Test {@link Assertions#clone()}.
   * <ul>
   *   <li>Given {@link Assertions} (default constructor).</li>
   *   <li>Then return Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link Assertions#clone()}
   */
  @Test
  public void testClone_givenAssertions_thenReturnChecked() throws CloneNotSupportedException {
    // Arrange and Act
    Object actualCloneResult = (new Assertions()).clone();

    // Assert
    assertTrue(actualCloneResult instanceof Assertions);
    assertEquals("Assertions", ((Assertions) actualCloneResult).getDataTypeName());
    Location location = ((Assertions) actualCloneResult).getLocation();
    assertNull(location.getFileName());
    assertNull(((Assertions) actualCloneResult).getDescription());
    assertNull(((Assertions) actualCloneResult).getProject());
    assertNull(((Assertions) actualCloneResult).getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, ((Assertions) actualCloneResult).size());
    assertFalse(((Assertions) actualCloneResult).isReference());
    assertTrue(((Assertions) actualCloneResult).isChecked());
  }

  /**
   * Test new {@link Assertions} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Assertions}
   */
  @Test
  public void testNewAssertions() {
    // Arrange and Act
    Assertions actualAssertions = new Assertions();

    // Assert
    assertEquals("Assertions", actualAssertions.getDataTypeName());
    Location location = actualAssertions.getLocation();
    assertNull(location.getFileName());
    assertNull(actualAssertions.getDescription());
    assertNull(actualAssertions.getProject());
    assertNull(actualAssertions.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualAssertions.size());
    assertFalse(actualAssertions.isReference());
    assertTrue(actualAssertions.isChecked());
  }
}
