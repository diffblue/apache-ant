package org.apache.tools.ant.taskdefs.optional.jsp.compilers;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import java.io.File;
import java.util.Iterator;
import java.util.Vector;
import org.apache.tools.ant.taskdefs.optional.jsp.Jasper41Mangler;
import org.apache.tools.ant.taskdefs.optional.jsp.JspC;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.Commandline.Argument;
import org.apache.tools.ant.types.CommandlineJava;
import org.junit.Test;

public class DefaultJspCompilerAdapterDiffblueTest {
  /**
   * Test {@link DefaultJspCompilerAdapter#logAndAddFilesToCompile(JspC, Vector, CommandlineJava)}.
   * <ul>
   *   <li>When {@link Vector#Vector()}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#logAndAddFilesToCompile(JspC, Vector, CommandlineJava)}
   */
  @Test
  public void testLogAndAddFilesToCompile_whenVector_thenCommandlineJavaJavaCommandSizeIsZero() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    JspC jspc = new JspC();
    Vector<String> compileList = new Vector<>();
    CommandlineJava cmd = new CommandlineJava();

    // Act
    jasperC.logAndAddFilesToCompile(jspc, compileList, cmd);

    // Assert that nothing has changed
    Commandline javaCommand = cmd.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, cmd.size());
    assertEquals(1, cmd.getCommandline().length);
    assertFalse(javaCommand.iterator().hasNext());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#setJspc(JspC)}.
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#setJspc(JspC)}
   */
  @Test
  public void testSetJspc() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    JspC owner = new JspC();

    // Act
    jasperC.setJspc(owner);

    // Assert
    assertNull(jasperC.getProject());
    assertSame(owner, jasperC.getJspc());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#getJspc()}.
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#getJspc()}
   */
  @Test
  public void testGetJspc() {
    // Arrange, Act and Assert
    assertNull((new JasperC(new Jasper41Mangler())).getJspc());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String, File)} with {@code cmd}, {@code argument}, {@code file}.
   * <ul>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String, File)}
   */
  @Test
  public void testAddArgWithCmdArgumentFile_thenCommandlineJavaJavaCommandSizeIsZero() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    CommandlineJava cmd = new CommandlineJava();

    // Act
    jasperC.addArg(cmd, "Argument", (File) null);

    // Assert that nothing has changed
    Commandline javaCommand = cmd.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, cmd.size());
    assertEquals(1, cmd.getCommandline().length);
    assertFalse(javaCommand.iterator().hasNext());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String, String)} with {@code cmd}, {@code argument}, {@code value}.
   * <ul>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String, String)}
   */
  @Test
  public void testAddArgWithCmdArgumentValue_thenCommandlineJavaJavaCommandSizeIsZero() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    CommandlineJava cmd = new CommandlineJava();

    // Act
    jasperC.addArg(cmd, "Argument", (String) null);

    // Assert that nothing has changed
    Commandline javaCommand = cmd.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, cmd.size());
    assertEquals(1, cmd.getCommandline().length);
    assertFalse(javaCommand.iterator().hasNext());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String)} with {@code cmd}, {@code argument}.
   * <ul>
   *   <li>When {@code Argument}.</li>
   *   <li>Then second element is {@code Argument}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String)}
   */
  @Test
  public void testAddArgWithCmdArgument_whenArgument_thenSecondElementIsArgument() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    CommandlineJava cmd = new CommandlineJava();

    // Act
    jasperC.addArg(cmd, "Argument");

    // Assert
    String[] commandline = cmd.getCommandline();
    assertEquals("Argument", commandline[1]);
    Commandline javaCommand = cmd.getJavaCommand();
    Iterator<Argument> iteratorResult = javaCommand.iterator();
    Argument nextResult = iteratorResult.next();
    assertNull(nextResult.getDescription());
    assertNull(nextResult.getProject());
    assertEquals(1, javaCommand.size());
    assertEquals(1, nextResult.getParts().length);
    assertEquals(2, cmd.size());
    assertEquals(2, commandline.length);
    assertFalse(iteratorResult.hasNext());
    assertArrayEquals(new String[]{"Argument"}, javaCommand.getArguments());
    assertArrayEquals(new String[]{"Argument"}, javaCommand.getCommandline());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String)} with {@code cmd}, {@code argument}.
   * <ul>
   *   <li>When empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String)}
   */
  @Test
  public void testAddArgWithCmdArgument_whenEmptyString() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    CommandlineJava cmd = new CommandlineJava();

    // Act
    jasperC.addArg(cmd, "");

    // Assert that nothing has changed
    Commandline javaCommand = cmd.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, cmd.size());
    assertEquals(1, cmd.getCommandline().length);
    assertFalse(javaCommand.iterator().hasNext());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String)} with {@code cmd}, {@code argument}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link CommandlineJava} (default constructor) JavaCommand size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#addArg(CommandlineJava, String)}
   */
  @Test
  public void testAddArgWithCmdArgument_whenNull_thenCommandlineJavaJavaCommandSizeIsZero() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    CommandlineJava cmd = new CommandlineJava();

    // Act
    jasperC.addArg(cmd, null);

    // Assert that nothing has changed
    Commandline javaCommand = cmd.getJavaCommand();
    assertEquals(0, javaCommand.size());
    assertEquals(0, javaCommand.getArguments().length);
    assertEquals(0, javaCommand.getCommandline().length);
    assertEquals(1, cmd.size());
    assertEquals(1, cmd.getCommandline().length);
    assertFalse(javaCommand.iterator().hasNext());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#implementsOwnDependencyChecking()}.
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#implementsOwnDependencyChecking()}
   */
  @Test
  public void testImplementsOwnDependencyChecking() {
    // Arrange, Act and Assert
    assertFalse((new JasperC(new Jasper41Mangler())).implementsOwnDependencyChecking());
  }

  /**
   * Test {@link DefaultJspCompilerAdapter#getProject()}.
   * <ul>
   *   <li>Given {@link JasperC#JasperC(JspMangler)} with mangler is {@link Jasper41Mangler} (default constructor) Jspc is {@link JspC} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultJspCompilerAdapter#getProject()}
   */
  @Test
  public void testGetProject_givenJasperCWithManglerIsJasper41ManglerJspcIsJspC_thenReturnNull() {
    // Arrange
    JasperC jasperC = new JasperC(new Jasper41Mangler());
    jasperC.setJspc(new JspC());

    // Act and Assert
    assertNull(jasperC.getProject());
  }
}
