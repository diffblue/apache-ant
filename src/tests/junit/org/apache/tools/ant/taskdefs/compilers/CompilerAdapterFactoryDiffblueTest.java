package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DummyTaskOk;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class CompilerAdapterFactoryDiffblueTest {
  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_CLASSIC}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_classic() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_CLASSIC, task,
        Path.systemBootClasspath) instanceof Javac13);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_GCJ}.</li>
   *   <li>Then return {@link Gcj}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_gcj_thenReturnGcj() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_GCJ, task,
        Path.systemBootClasspath) instanceof Gcj);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JAVAC_10_PLUS}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_javac_10_plus() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JAVAC_10_PLUS, task,
        Path.systemBootClasspath) instanceof Javac13);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JIKES}.</li>
   *   <li>Then return {@link Jikes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_jikes_thenReturnJikes() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JIKES, task,
        Path.systemBootClasspath) instanceof Jikes);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JVC_ALIAS}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_jvc_alias() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JVC_ALIAS, task,
        Path.systemBootClasspath) instanceof Jvc);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JVC}.</li>
   *   <li>Then return {@link Jvc}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_jvc_thenReturnJvc() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JVC, task,
        Path.systemBootClasspath) instanceof Jvc);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_KJC}.</li>
   *   <li>Then return {@link Kjc}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_kjc_thenReturnKjc() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_KJC, task,
        Path.systemBootClasspath) instanceof Kjc);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_SYMANTEC_ALIAS}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_symantec_alias() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_SYMANTEC_ALIAS, task,
        Path.systemBootClasspath) instanceof Sj);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_SYMANTEC}.</li>
   *   <li>Then return {@link Sj}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenCompiler_symantec_thenReturnSj() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_SYMANTEC, task,
        Path.systemBootClasspath) instanceof Sj);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task, Path)} with {@code compilerType}, {@code task}, {@code classpath}.
   * <ul>
   *   <li>When {@link TaskAdapter#TaskAdapter()} Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task, Path)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskClasspath_whenTaskAdapterProjectIsNull() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(null);

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_CLASSIC, task,
        Path.systemBootClasspath) instanceof Javac13);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Given {@code null}.</li>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JIKES}.</li>
   *   <li>Then return {@link Jikes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_givenNull_whenCompiler_jikes_thenReturnJikes() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(null);

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JIKES, task) instanceof Jikes);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_GCJ}.</li>
   *   <li>Then return {@link Gcj}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_givenProject_whenCompiler_gcj_thenReturnGcj() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_GCJ, task) instanceof Gcj);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JVC}.</li>
   *   <li>Then return {@link Jvc}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_givenProject_whenCompiler_jvc_thenReturnJvc() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JVC, task) instanceof Jvc);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_KJC}.</li>
   *   <li>Then return {@link Kjc}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_givenProject_whenCompiler_kjc_thenReturnKjc() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_KJC, task) instanceof Kjc);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_CLASSIC}.</li>
   *   <li>Then return {@link Javac13}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenCompiler_classic_thenReturnJavac13() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_CLASSIC, task) instanceof Javac13);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_CLASSIC}.</li>
   *   <li>Then return {@link Javac13}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenCompiler_classic_thenReturnJavac132() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(null);

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_CLASSIC, task) instanceof Javac13);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JAVAC_10_PLUS}.</li>
   *   <li>Then return {@link Javac13}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenCompiler_javac_10_plus_thenReturnJavac13() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(
        CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JAVAC_10_PLUS, task) instanceof Javac13);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JIKES}.</li>
   *   <li>Then return {@link Jikes}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenCompiler_jikes_thenReturnJikes() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JIKES, task) instanceof Jikes);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JVC_ALIAS}.</li>
   *   <li>Then return {@link Jvc}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenCompiler_jvc_alias_thenReturnJvc() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_JVC_ALIAS, task) instanceof Jvc);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_SYMANTEC_ALIAS}.</li>
   *   <li>Then return {@link Sj}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenCompiler_symantec_alias_thenReturnSj() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_SYMANTEC_ALIAS, task) instanceof Sj);
  }

  /**
   * Test {@link CompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_SYMANTEC}.</li>
   *   <li>Then return {@link Sj}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenCompiler_symantec_thenReturnSj() throws BuildException {
    // Arrange
    DummyTaskOk task = new DummyTaskOk();
    task.setProject(new Project());

    // Act and Assert
    assertTrue(CompilerAdapterFactory.getCompiler(CompilerAdapterFactory.COMPILER_SYMANTEC, task) instanceof Sj);
  }

  /**
   * Test {@link CompilerAdapterFactory#isForkedJavac(String)}.
   * <ul>
   *   <li>When {@code Compiler Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isForkedJavac(String)}
   */
  @Test
  public void testIsForkedJavac_whenCompilerName() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isForkedJavac("Compiler Name"));
  }

  /**
   * Test {@link CompilerAdapterFactory#isForkedJavac(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isForkedJavac(String)}
   */
  @Test
  public void testIsForkedJavac_whenNull() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isForkedJavac(null));
  }

  /**
   * Test {@link CompilerAdapterFactory#isJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code Compiler Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isJdkCompiler(String)}
   */
  @Test
  public void testIsJdkCompiler_whenCompilerName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isJdkCompiler("Compiler Name"));
  }

  /**
   * Test {@link CompilerAdapterFactory#isJdkCompiler(String)}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_CLASSIC}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isJdkCompiler(String)}
   */
  @Test
  public void testIsJdkCompiler_whenCompiler_classic_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(CompilerAdapterFactory.isJdkCompiler(CompilerAdapterFactory.COMPILER_CLASSIC));
  }

  /**
   * Test {@link CompilerAdapterFactory#isJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isJdkCompiler(String)}
   */
  @Test
  public void testIsJdkCompiler_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isJdkCompiler(null));
  }

  /**
   * Test {@link CompilerAdapterFactory#isJdkCompilerNickname(String)}.
   * <ul>
   *   <li>When {@code Compiler Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isJdkCompilerNickname(String)}
   */
  @Test
  public void testIsJdkCompilerNickname_whenCompilerName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isJdkCompilerNickname("Compiler Name"));
  }

  /**
   * Test {@link CompilerAdapterFactory#isJdkCompilerNickname(String)}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_CLASSIC}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isJdkCompilerNickname(String)}
   */
  @Test
  public void testIsJdkCompilerNickname_whenCompiler_classic_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(CompilerAdapterFactory.isJdkCompilerNickname(CompilerAdapterFactory.COMPILER_CLASSIC));
  }

  /**
   * Test {@link CompilerAdapterFactory#isJdkCompilerNickname(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isJdkCompilerNickname(String)}
   */
  @Test
  public void testIsJdkCompilerNickname_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isJdkCompilerNickname(null));
  }

  /**
   * Test {@link CompilerAdapterFactory#isClassicJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code Compiler Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isClassicJdkCompiler(String)}
   */
  @Test
  public void testIsClassicJdkCompiler_whenCompilerName() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isClassicJdkCompiler("Compiler Name"));
  }

  /**
   * Test {@link CompilerAdapterFactory#isClassicJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isClassicJdkCompiler(String)}
   */
  @Test
  public void testIsClassicJdkCompiler_whenNull() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isClassicJdkCompiler(null));
  }

  /**
   * Test {@link CompilerAdapterFactory#isModernJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code Compiler Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isModernJdkCompiler(String)}
   */
  @Test
  public void testIsModernJdkCompiler_whenCompilerName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isModernJdkCompiler("Compiler Name"));
  }

  /**
   * Test {@link CompilerAdapterFactory#isModernJdkCompiler(String)}.
   * <ul>
   *   <li>When {@link CompilerAdapterFactory#COMPILER_JAVAC_10_PLUS}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isModernJdkCompiler(String)}
   */
  @Test
  public void testIsModernJdkCompiler_whenCompiler_javac_10_plus_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue(CompilerAdapterFactory.isModernJdkCompiler(CompilerAdapterFactory.COMPILER_JAVAC_10_PLUS));
  }

  /**
   * Test {@link CompilerAdapterFactory#isModernJdkCompiler(String)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CompilerAdapterFactory#isModernJdkCompiler(String)}
   */
  @Test
  public void testIsModernJdkCompiler_whenNull_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse(CompilerAdapterFactory.isModernJdkCompiler(null));
  }
}
