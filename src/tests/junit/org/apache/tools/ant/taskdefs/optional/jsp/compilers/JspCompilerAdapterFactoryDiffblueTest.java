package org.apache.tools.ant.taskdefs.optional.jsp.compilers;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.taskdefs.optional.jsp.Jasper41Mangler;
import org.apache.tools.ant.taskdefs.optional.jsp.JspNameMangler;
import org.junit.Test;

public class JspCompilerAdapterFactoryDiffblueTest {
  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)} with {@code compilerType}, {@code task}, {@code loader}.
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskLoader() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    AntClassLoader loader = new AntClassLoader();
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task, loader));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)} with {@code compilerType}, {@code task}, {@code loader}.
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskLoader2() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    AntClassLoader loader = new AntClassLoader();
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "jasper").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task, loader));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)} with {@code compilerType}, {@code task}, {@code loader}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskLoader_givenJavaLangObject() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("jasper", typeClass);

    AntClassLoader loader = new AntClassLoader();
    loader.setProject(project);
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task, loader));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)} with {@code compilerType}, {@code task}, {@code loader}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskLoader_givenProject() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    AntClassLoader loader = new AntClassLoader();
    loader.setProject(new Project());
    loader.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task, loader));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)} with {@code compilerType}, {@code task}, {@code loader}.
   * <ul>
   *   <li>Then {@link JasperC#mangler} return {@link Jasper41Mangler}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskLoader_thenManglerReturnJasper41Mangler() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act
    JspCompilerAdapter actualCompiler = JspCompilerAdapterFactory.getCompiler("jasper41", task, new AntClassLoader());

    // Assert
    assertTrue(((JasperC) actualCompiler).mangler instanceof Jasper41Mangler);
    assertTrue(actualCompiler instanceof JasperC);
    assertNull(((JasperC) actualCompiler).getJspc());
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)} with {@code compilerType}, {@code task}, {@code loader}.
   * <ul>
   *   <li>Then {@link JasperC#mangler} return {@link JspNameMangler}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskLoader_thenManglerReturnJspNameMangler() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act
    JspCompilerAdapter actualCompiler = JspCompilerAdapterFactory.getCompiler("jasper", task, new AntClassLoader());

    // Assert
    assertTrue(((JasperC) actualCompiler).mangler instanceof JspNameMangler);
    assertTrue(actualCompiler instanceof JasperC);
    assertNull(((JasperC) actualCompiler).getJspc());
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)} with {@code compilerType}, {@code task}, {@code loader}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task, AntClassLoader)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTaskLoader_whenAntClassLoader() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task, new AntClassLoader()));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("jasper", typeClass);
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_givenProjectAddBuildListenerAntClassLoader() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    TaskAdapter task = new TaskAdapter();
    task.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_givenProject_thenThrowBuildException() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> JspCompilerAdapterFactory.getCompiler("Compiler Type", task));
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>Then {@link JasperC#mangler} return {@link Jasper41Mangler}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_thenManglerReturnJasper41Mangler() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    JspCompilerAdapter actualCompiler = JspCompilerAdapterFactory.getCompiler("jasper41", task);

    // Assert
    assertTrue(((JasperC) actualCompiler).mangler instanceof Jasper41Mangler);
    assertTrue(actualCompiler instanceof JasperC);
    assertNull(((JasperC) actualCompiler).getJspc());
    assertEquals(1, task.getProject().getBuildListeners().size());
  }

  /**
   * Test {@link JspCompilerAdapterFactory#getCompiler(String, Task)} with {@code compilerType}, {@code task}.
   * <ul>
   *   <li>When {@code jasper}.</li>
   *   <li>Then {@link JasperC#mangler} return {@link JspNameMangler}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JspCompilerAdapterFactory#getCompiler(String, Task)}
   */
  @Test
  public void testGetCompilerWithCompilerTypeTask_whenJasper_thenManglerReturnJspNameMangler() throws BuildException {
    // Arrange
    TaskAdapter task = new TaskAdapter();
    task.setProject(new Project());

    // Act
    JspCompilerAdapter actualCompiler = JspCompilerAdapterFactory.getCompiler("jasper", task);

    // Assert
    assertTrue(((JasperC) actualCompiler).mangler instanceof JspNameMangler);
    assertTrue(actualCompiler instanceof JasperC);
    assertNull(((JasperC) actualCompiler).getJspc());
    assertEquals(1, task.getProject().getBuildListeners().size());
  }
}
