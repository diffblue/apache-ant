package org.apache.tools.ant.taskdefs.optional.javah;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.optional.Javah;
import org.apache.tools.ant.types.Commandline;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class SunJavahDiffblueTest {
  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Javah javah = new Javah();
    javah.setDestdir(null);
    javah.setOutputFile(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    javah.setClasspath(null);
    javah.setVerbose(false);
    javah.setOld(false);
    javah.setForce(false);
    javah.setStubs(false);
    javah.setBootclasspath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile2() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Javah javah = new Javah();
    javah.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    javah.setOutputFile(null);
    javah.setClasspath(null);
    javah.setVerbose(false);
    javah.setOld(false);
    javah.setForce(false);
    javah.setStubs(false);
    javah.setBootclasspath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>Given {@code ignore}.</li>
   *   <li>When {@link Javah} (default constructor) Class is {@code ignore}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_givenIgnore_whenJavahClassIsIgnore_thenThrowBuildException() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Javah javah = new Javah();
    javah.setClass("ignore");

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link Javah} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_givenJavaLangObject_whenJavahProjectIsProject() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("build.sysclasspath", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javah javah = new Javah();
    javah.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@link Javah} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_givenProjectAddBuildListenerAntClassLoader_whenJavahProjectIsProject() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javah javah = new Javah();
    javah.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Javah} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_givenProject_whenJavahProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Javah javah = new Javah();
    javah.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>Given {@link Path#systemBootClasspath}.</li>
   *   <li>When {@link Javah} (default constructor) Bootclasspath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_givenSystemBootClasspath_whenJavahBootclasspathIsSystemBootClasspath() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Javah javah = new Javah();
    javah.setDestdir(null);
    javah.setOutputFile(null);
    javah.setClasspath(null);
    javah.setVerbose(false);
    javah.setOld(false);
    javah.setForce(false);
    javah.setStubs(false);
    javah.setBootclasspath(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>Given {@link Path#systemBootClasspath}.</li>
   *   <li>When {@link Javah} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_givenSystemBootClasspath_whenJavahClasspathIsSystemBootClasspath() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Javah javah = new Javah();
    javah.setDestdir(null);
    javah.setOutputFile(null);
    javah.setClasspath(Path.systemBootClasspath);
    javah.setVerbose(false);
    javah.setOld(false);
    javah.setForce(false);
    javah.setStubs(false);
    javah.setBootclasspath(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Javah} (default constructor) Stubs is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_givenTrue_whenJavahStubsIsTrue_thenThrowBuildException() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    Javah javah = new Javah();
    javah.setStubs(true);
    javah.addFileSet(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(javah));
  }

  /**
   * Test {@link SunJavah#compile(Javah)}.
   * <ul>
   *   <li>When {@link Javah} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#compile(Javah)}
   */
  @Test
  public void testCompile_whenJavah_thenThrowBuildException() throws BuildException {
    // Arrange
    SunJavah sunJavah = new SunJavah();

    // Act and Assert
    assertThrows(BuildException.class, () -> sunJavah.compile(new Javah()));
  }

  /**
   * Test {@link SunJavah#setupJavahCommand(Javah)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link Javah} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#setupJavahCommand(Javah)}
   */
  @Test
  public void testSetupJavahCommand_givenJavaLangObject_whenJavahProjectIsProject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("build.sysclasspath", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javah javah = new Javah();
    javah.setProject(project);

    // Act
    Commandline actualSetupJavahCommandResult = SunJavah.setupJavahCommand(javah);

    // Assert
    assertEquals(0, actualSetupJavahCommandResult.size());
    assertEquals(0, actualSetupJavahCommandResult.getArguments().length);
    assertEquals(0, actualSetupJavahCommandResult.getCommandline().length);
    assertFalse(actualSetupJavahCommandResult.iterator().hasNext());
  }

  /**
   * Test {@link SunJavah#setupJavahCommand(Javah)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#setupJavahCommand(Javah)}
   */
  @Test
  public void testSetupJavahCommand_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javah javah = new Javah();
    javah.setProject(project);

    // Act
    Commandline actualSetupJavahCommandResult = SunJavah.setupJavahCommand(javah);

    // Assert
    assertEquals(0, actualSetupJavahCommandResult.size());
    assertEquals(0, actualSetupJavahCommandResult.getArguments().length);
    assertEquals(0, actualSetupJavahCommandResult.getCommandline().length);
    assertFalse(actualSetupJavahCommandResult.iterator().hasNext());
  }

  /**
   * Test {@link SunJavah#setupJavahCommand(Javah)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Javah} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#setupJavahCommand(Javah)}
   */
  @Test
  public void testSetupJavahCommand_givenProject_whenJavahProjectIsProject() {
    // Arrange
    Javah javah = new Javah();
    javah.setProject(new Project());

    // Act
    Commandline actualSetupJavahCommandResult = SunJavah.setupJavahCommand(javah);

    // Assert
    assertEquals(0, actualSetupJavahCommandResult.size());
    assertEquals(0, actualSetupJavahCommandResult.getArguments().length);
    assertEquals(0, actualSetupJavahCommandResult.getCommandline().length);
    assertFalse(actualSetupJavahCommandResult.iterator().hasNext());
  }

  /**
   * Test {@link SunJavah#setupJavahCommand(Javah)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Javah} (default constructor) Stubs is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#setupJavahCommand(Javah)}
   */
  @Test
  public void testSetupJavahCommand_givenTrue_whenJavahStubsIsTrue_thenThrowBuildException() {
    // Arrange
    Javah javah = new Javah();
    javah.setStubs(true);
    javah.addFileSet(new FileSet());

    // Act and Assert
    assertThrows(BuildException.class, () -> SunJavah.setupJavahCommand(javah));
  }

  /**
   * Test {@link SunJavah#setupJavahCommand(Javah)}.
   * <ul>
   *   <li>When {@link Javah} (default constructor).</li>
   *   <li>Then return size is zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link SunJavah#setupJavahCommand(Javah)}
   */
  @Test
  public void testSetupJavahCommand_whenJavah_thenReturnSizeIsZero() {
    // Arrange and Act
    Commandline actualSetupJavahCommandResult = SunJavah.setupJavahCommand(new Javah());

    // Assert
    assertEquals(0, actualSetupJavahCommandResult.size());
    assertEquals(0, actualSetupJavahCommandResult.getArguments().length);
    assertEquals(0, actualSetupJavahCommandResult.getCommandline().length);
    assertFalse(actualSetupJavahCommandResult.iterator().hasNext());
  }
}
