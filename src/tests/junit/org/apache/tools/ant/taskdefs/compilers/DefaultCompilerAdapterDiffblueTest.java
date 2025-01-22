package org.apache.tools.ant.taskdefs.compilers;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.util.Map;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.apache.tools.ant.taskdefs.Javac;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class DefaultCompilerAdapterDiffblueTest {
  /**
   * Test {@link DefaultCompilerAdapter#setJavac(Javac)}.
   * <ul>
   *   <li>Given {@code ant.build.javac.target}.</li>
   *   <li>Then {@link Gcj} (default constructor) {@link DefaultCompilerAdapter#target} is {@code ant.build.javac.target}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#setJavac(Javac)}
   */
  @Test
  public void testSetJavac_givenAntBuildJavacTarget_thenGcjTargetIsAntBuildJavacTarget() {
    // Arrange
    Gcj gcj = new Gcj();

    Javac attributes = new Javac();
    attributes.setTarget("ant.build.javac.target");

    // Act
    gcj.setJavac(attributes);

    // Assert
    assertEquals("ant.build.javac.target", gcj.target);
    assertNull(gcj.getBootClassPath().getProject());
    assertNull(gcj.getCompileClasspath().getProject());
    assertNull(gcj.getModulepath().getProject());
    assertNull(gcj.getModulesourcepath().getProject());
    assertNull(gcj.getUpgrademodulepath().getProject());
    assertNull(gcj.getProject());
  }

  /**
   * Test {@link DefaultCompilerAdapter#setJavac(Javac)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then {@link Gcj} (default constructor) NoDebugArgument is {@code -g:none}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#setJavac(Javac)}
   */
  @Test
  public void testSetJavac_givenJavaLangObject_thenGcjNoDebugArgumentIsGNone() {
    // Arrange
    Gcj gcj = new Gcj();

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    Javac attributes = new Javac();
    attributes.setProject(project);

    // Act
    gcj.setJavac(attributes);

    // Assert
    assertEquals("-g:none", gcj.getNoDebugArgument());
    assertNull(gcj.target);
    assertSame(project, gcj.getBootClassPath().getProject());
    assertSame(project, gcj.getCompileClasspath().getProject());
    assertSame(project, gcj.getModulepath().getProject());
    assertSame(project, gcj.getModulesourcepath().getProject());
    assertSame(project, gcj.getUpgrademodulepath().getProject());
    assertSame(project, gcj.getProject());
    Location expectedLocation = gcj.location;
    assertSame(expectedLocation, attributes.getProject().getGlobalFilterSet().getLocation());
  }

  /**
   * Test {@link DefaultCompilerAdapter#setJavac(Javac)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#setJavac(Javac)}
   */
  @Test
  public void testSetJavac_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Gcj gcj = new Gcj();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Javac attributes = new Javac();
    attributes.setProject(project);

    // Act
    gcj.setJavac(attributes);

    // Assert
    assertEquals("-g:none", gcj.getNoDebugArgument());
    assertNull(gcj.target);
    assertSame(project, gcj.getBootClassPath().getProject());
    assertSame(project, gcj.getCompileClasspath().getProject());
    assertSame(project, gcj.getModulepath().getProject());
    assertSame(project, gcj.getModulesourcepath().getProject());
    assertSame(project, gcj.getUpgrademodulepath().getProject());
    assertSame(project, gcj.getProject());
    Location expectedLocation = gcj.location;
    assertSame(expectedLocation, attributes.getProject().getGlobalFilterSet().getLocation());
  }

  /**
   * Test {@link DefaultCompilerAdapter#setJavac(Javac)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>Then {@link Gcj} (default constructor) NoDebugArgument is {@code -g:none}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#setJavac(Javac)}
   */
  @Test
  public void testSetJavac_givenProject_thenGcjNoDebugArgumentIsGNone() {
    // Arrange
    Gcj gcj = new Gcj();

    Javac attributes = new Javac();
    Project project = new Project();
    attributes.setProject(project);

    // Act
    gcj.setJavac(attributes);

    // Assert
    assertEquals("-g:none", gcj.getNoDebugArgument());
    assertNull(gcj.target);
    assertSame(project, gcj.getBootClassPath().getProject());
    assertSame(project, gcj.getCompileClasspath().getProject());
    assertSame(project, gcj.getModulepath().getProject());
    assertSame(project, gcj.getModulesourcepath().getProject());
    assertSame(project, gcj.getUpgrademodulepath().getProject());
    assertSame(project, gcj.getProject());
    Location expectedLocation = gcj.location;
    assertSame(expectedLocation, attributes.getProject().getGlobalFilterSet().getLocation());
  }

  /**
   * Test {@link DefaultCompilerAdapter#setJavac(Javac)}.
   * <ul>
   *   <li>Then {@link Javac} (default constructor) Project CopyOfTargets size is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#setJavac(Javac)}
   */
  @Test
  public void testSetJavac_thenJavacProjectCopyOfTargetsSizeIsOne() throws BuildException {
    // Arrange
    Gcj gcj = new Gcj();

    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    Javac attributes = new Javac();
    attributes.setProject(project);

    // Act
    gcj.setJavac(attributes);

    // Assert
    assertEquals("-g:none", gcj.getNoDebugArgument());
    assertNull(gcj.target);
    Project project2 = attributes.getProject();
    Map<String, Target> copyOfTargets = project2.getCopyOfTargets();
    assertEquals(1, copyOfTargets.size());
    assertSame(project, gcj.getBootClassPath().getProject());
    assertSame(project, gcj.getCompileClasspath().getProject());
    assertSame(project, gcj.getModulepath().getProject());
    assertSame(project, gcj.getModulesourcepath().getProject());
    assertSame(project, gcj.getUpgrademodulepath().getProject());
    assertSame(project, gcj.getProject());
    Location location = gcj.location;
    assertSame(location, project2.getGlobalFilterSet().getLocation());
    assertSame(location, copyOfTargets.get("ant.PropertyHelper").getLocation());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getJavac()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getJavac()}
   */
  @Test
  public void testGetJavac() {
    // Arrange, Act and Assert
    assertNull((new Gcj()).getJavac());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getSupportedFileExtensions()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getSupportedFileExtensions()}
   */
  @Test
  public void testGetSupportedFileExtensions() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"java"}, (new Gcj()).getSupportedFileExtensions());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getProject()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getProject()}
   */
  @Test
  public void testGetProject() {
    // Arrange, Act and Assert
    assertNull((new Gcj()).getProject());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getCompileClasspath()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getCompileClasspath()}
   */
  @Test
  public void testGetCompileClasspath() {
    // Arrange and Act
    Path actualCompileClasspath = (new Gcj()).getCompileClasspath();

    // Assert
    Location location = actualCompileClasspath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCompileClasspath.getDescription());
    assertNull(actualCompileClasspath.getProject());
    assertNull(actualCompileClasspath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualCompileClasspath.size());
    assertFalse(actualCompileClasspath.isReference());
    assertTrue(actualCompileClasspath.isEmpty());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getModulepath()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getModulepath()}
   */
  @Test
  public void testGetModulepath() {
    // Arrange and Act
    Path actualModulepath = (new Gcj()).getModulepath();

    // Assert
    Location location = actualModulepath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualModulepath.getDescription());
    assertNull(actualModulepath.getProject());
    assertNull(actualModulepath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualModulepath.size());
    assertFalse(actualModulepath.isReference());
    assertTrue(actualModulepath.isEmpty());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getUpgrademodulepath()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getUpgrademodulepath()}
   */
  @Test
  public void testGetUpgrademodulepath() {
    // Arrange and Act
    Path actualUpgrademodulepath = (new Gcj()).getUpgrademodulepath();

    // Assert
    Location location = actualUpgrademodulepath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualUpgrademodulepath.getDescription());
    assertNull(actualUpgrademodulepath.getProject());
    assertNull(actualUpgrademodulepath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualUpgrademodulepath.size());
    assertFalse(actualUpgrademodulepath.isReference());
    assertTrue(actualUpgrademodulepath.isEmpty());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getModulesourcepath()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getModulesourcepath()}
   */
  @Test
  public void testGetModulesourcepath() {
    // Arrange and Act
    Path actualModulesourcepath = (new Gcj()).getModulesourcepath();

    // Assert
    Location location = actualModulesourcepath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualModulesourcepath.getDescription());
    assertNull(actualModulesourcepath.getProject());
    assertNull(actualModulesourcepath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualModulesourcepath.size());
    assertFalse(actualModulesourcepath.isReference());
    assertTrue(actualModulesourcepath.isEmpty());
  }

  /**
   * Test {@link DefaultCompilerAdapter#getBootClassPath()}.
   * <p>
   * Method under test: {@link DefaultCompilerAdapter#getBootClassPath()}
   */
  @Test
  public void testGetBootClassPath() {
    // Arrange and Act
    Path actualBootClassPath = (new Gcj()).getBootClassPath();

    // Assert
    Location location = actualBootClassPath.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBootClassPath.getDescription());
    assertNull(actualBootClassPath.getProject());
    assertNull(actualBootClassPath.getRefid());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(0, actualBootClassPath.size());
    assertFalse(actualBootClassPath.isReference());
    assertTrue(actualBootClassPath.isEmpty());
  }
}
