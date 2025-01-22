package org.apache.tools.ant.taskdefs.optional.depend;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.types.DirSet;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.junit.Test;

public class DependDiffblueTest {
  /**
   * Test {@link Depend#setClasspath(Path)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then {@link Depend} (default constructor) RuntimeConfigurableWrapper AttributeMap Empty.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_whenNull_thenDependRuntimeConfigurableWrapperAttributeMapEmpty() {
    // Arrange
    Depend depend = new Depend();
    depend.setClasspath(Path.systemBootClasspath);

    // Act
    depend.setClasspath(null);

    // Assert that nothing has changed
    assertTrue(depend.getRuntimeConfigurableWrapper().getAttributeMap().isEmpty());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Depend#setCache(File)}
   *   <li>{@link Depend#setClosure(boolean)}
   *   <li>{@link Depend#setDestDir(Path)}
   *   <li>{@link Depend#setDump(boolean)}
   *   <li>{@link Depend#setSrcdir(Path)}
   *   <li>{@link Depend#setWarnOnRmiStubs(boolean)}
   *   <li>{@link Depend#getClasspath()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Depend depend = new Depend();

    // Act
    depend.setCache(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());
    depend.setClosure(true);
    depend.setDestDir(Path.systemBootClasspath);
    depend.setDump(true);
    depend.setSrcdir(Path.systemBootClasspath);
    depend.setWarnOnRmiStubs(true);

    // Assert
    assertNull(depend.getClasspath());
  }

  /**
   * Test {@link Depend#createClasspath()}.
   * <ul>
   *   <li>Given {@link Depend} (default constructor).</li>
   *   <li>Then {@link Depend} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#createClasspath()}
   */
  @Test
  public void testCreateClasspath_givenDepend_thenDependClasspathDescriptionIsNull() {
    // Arrange
    Depend depend = new Depend();

    // Act
    Path actualCreateClasspathResult = depend.createClasspath();

    // Assert
    Path classpath = depend.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link Depend#createClasspath()}.
   * <ul>
   *   <li>Then {@link Depend} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenDependClasspathIsSystemBootClasspath() {
    // Arrange
    Depend depend = new Depend();
    depend.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = depend.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, depend.getClasspath());
  }

  /**
   * Test {@link Depend#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Given {@link Depend} (default constructor).</li>
   *   <li>Then {@link Depend} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_givenDepend_thenDependClasspathProjectIsNull() {
    // Arrange
    Depend depend = new Depend();

    // Act
    depend.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = depend.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link Depend#execute()}.
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute() throws BuildException {
    // Arrange
    Depend depend = new Depend();
    depend.setSrcdir(new Path(new Project(), "No directory specified for %s."));

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute2() throws BuildException {
    // Arrange
    Path srcPath = new Path(new Project(), "No directory specified for %s.");
    srcPath.addFileset(new FileSet());

    Depend depend = new Depend();
    depend.setSrcdir(srcPath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Depend} (default constructor) Srcdir is {@link Path#Path(Project)} with project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenDependSrcdirIsPathWithProjectIsProject_thenThrowBuildException() throws BuildException {
    // Arrange
    Depend depend = new Depend();
    depend.setSrcdir(new Path(new Project()));

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Depend} (default constructor) Srcdir is {@link Path#systemBootClasspath}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenDependSrcdirIsSystemBootClasspath_thenThrowBuildException() throws BuildException {
    // Arrange
    Depend depend = new Depend();
    depend.setSrcdir(Path.systemBootClasspath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Depend} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenDepend_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Depend()).execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenJavaLangObject_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);

    Path srcPath = new Path(project);
    srcPath.addFileset(new FileSet());

    Depend depend = new Depend();
    depend.setSrcdir(srcPath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@code null} addFileset {@link FileSet#FileSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenPathWithProjectIsNullAddFilesetFileSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Path srcPath = new Path(null);
    srcPath.addFileset(new FileSet());

    Depend depend = new Depend();
    depend.setSrcdir(srcPath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addDirset {@link DirSet#DirSet()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenPathWithProjectIsProjectAddDirsetDirSet_thenThrowBuildException() throws BuildException {
    // Arrange
    Path srcPath = new Path(new Project());
    srcPath.addDirset(new DirSet());
    srcPath.addFileset(new FileSet());

    Depend depend = new Depend();
    depend.setSrcdir(srcPath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFilelist {@link FileList#FileList()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenPathWithProjectIsProjectAddFilelistFileList() throws BuildException {
    // Arrange
    Path srcPath = new Path(new Project());
    srcPath.addFilelist(new FileList());
    srcPath.addFileset(new FileSet());

    Depend depend = new Depend();
    depend.setSrcdir(srcPath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Path#Path(Project)} with project is {@link Project} (default constructor) addFileset {@link FileSet#FileSet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenPathWithProjectIsProjectAddFilesetFileSet() throws BuildException {
    // Arrange
    Path srcPath = new Path(new Project());
    srcPath.addFileset(new FileSet());

    Depend depend = new Depend();
    depend.setSrcdir(srcPath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test {@link Depend#execute()}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Depend#execute()}
   */
  @Test
  public void testExecute_givenProjectAddBuildListenerAntClassLoader_thenThrowBuildException() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Path srcPath = new Path(project);
    srcPath.addFileset(new FileSet());

    Depend depend = new Depend();
    depend.setSrcdir(srcPath);

    // Act and Assert
    assertThrows(BuildException.class, () -> depend.execute());
  }

  /**
   * Test new {@link Depend} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Depend}
   */
  @Test
  public void testNewDepend() {
    // Arrange and Act
    Depend actualDepend = new Depend();

    // Assert
    Location location = actualDepend.getLocation();
    assertNull(location.getFileName());
    assertNull(actualDepend.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualDepend.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualDepend.getTaskName());
    assertNull(actualDepend.getTaskType());
    assertNull(actualDepend.getProject());
    assertNull(actualDepend.getOwningTarget());
    assertNull(actualDepend.getClasspath());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertFalse(actualDepend.hasSelectors());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualDepend, runtimeConfigurableWrapper.getProxy());
  }
}
