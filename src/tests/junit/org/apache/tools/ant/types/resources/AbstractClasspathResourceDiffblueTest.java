package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Reference;
import org.apache.tools.ant.types.resources.AbstractClasspathResource.ClassLoaderWithFlag;
import org.junit.Test;

public class AbstractClasspathResourceDiffblueTest {
  /**
   * Test ClassLoaderWithFlag {@link ClassLoaderWithFlag#getLoader()}.
   * <p>
   * Method under test: {@link ClassLoaderWithFlag#getLoader()}
   */
  @Test
  public void testClassLoaderWithFlagGetLoader() {
    // Arrange
    AntClassLoader l = new AntClassLoader();

    // Act
    ClassLoader actualLoader = (new ClassLoaderWithFlag(l, true)).getLoader();

    // Assert
    assertNotNull(actualLoader);
    assertSame(l, actualLoader);
  }

  /**
   * Test ClassLoaderWithFlag {@link ClassLoaderWithFlag#needsCleanup()}.
   * <ul>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassLoaderWithFlag#needsCleanup()}
   */
  @Test
  public void testClassLoaderWithFlagNeedsCleanup_thenReturnFalse() throws MalformedURLException {
    // Arrange, Act and Assert
    assertFalse((new ClassLoaderWithFlag(
        new URLClassLoader(new URL[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()}),
        true)).needsCleanup());
  }

  /**
   * Test ClassLoaderWithFlag {@link ClassLoaderWithFlag#needsCleanup()}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassLoaderWithFlag#needsCleanup()}
   */
  @Test
  public void testClassLoaderWithFlagNeedsCleanup_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new ClassLoaderWithFlag(new AntClassLoader(), true)).needsCleanup());
  }

  /**
   * Test ClassLoaderWithFlag {@link ClassLoaderWithFlag#ClassLoaderWithFlag(ClassLoader, boolean)}.
   * <ul>
   *   <li>Then return Loader is not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassLoaderWithFlag#ClassLoaderWithFlag(ClassLoader, boolean)}
   */
  @Test
  public void testClassLoaderWithFlagNewClassLoaderWithFlag_thenReturnLoaderIsNotNull() {
    // Arrange, Act and Assert
    assertNotNull((new ClassLoaderWithFlag(new AntClassLoader(), true)).getLoader());
  }

  /**
   * Test ClassLoaderWithFlag {@link ClassLoaderWithFlag#ClassLoaderWithFlag(ClassLoader, boolean)}.
   * <ul>
   *   <li>When {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassLoaderWithFlag#ClassLoaderWithFlag(ClassLoader, boolean)}
   */
  @Test
  public void testClassLoaderWithFlagNewClassLoaderWithFlag_whenFalse() {
    // Arrange, Act and Assert
    assertNotNull((new ClassLoaderWithFlag(new AntClassLoader(), false)).getLoader());
  }

  /**
   * Test ClassLoaderWithFlag {@link ClassLoaderWithFlag#ClassLoaderWithFlag(ClassLoader, boolean)}.
   * <ul>
   *   <li>When {@code null}.</li>
   *   <li>Then return Loader is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ClassLoaderWithFlag#ClassLoaderWithFlag(ClassLoader, boolean)}
   */
  @Test
  public void testClassLoaderWithFlagNewClassLoaderWithFlag_whenNull_thenReturnLoaderIsNull() {
    // Arrange, Act and Assert
    assertNull((new ClassLoaderWithFlag(null, true)).getLoader());
  }

  /**
   * Test {@link AbstractClasspathResource#setClasspath(Path)}.
   * <p>
   * Method under test: {@link AbstractClasspathResource#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    Path classpath = Path.systemBootClasspath;

    // Act
    javaConstantResource.setClasspath(classpath);

    // Assert
    Path expectedClasspath = classpath.systemBootClasspath;
    assertSame(expectedClasspath, javaConstantResource.getClasspath());
  }

  /**
   * Test {@link AbstractClasspathResource#setClasspath(Path)}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor) Classpath is {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_givenJavaConstantResourceClasspathIsPathWithPIsProjectAndPath() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    Project p = new Project();
    javaConstantResource.setClasspath(new Path(p, "Path"));
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    // Act
    javaConstantResource.setClasspath(classpath);

    // Assert
    assertSame(p, classpath.getProject());
  }

  /**
   * Test {@link AbstractClasspathResource#setClasspath(Path)}.
   * <ul>
   *   <li>Then {@link Path#systemBootClasspath} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#setClasspath(Path)}
   */
  @Test
  public void testSetClasspath_thenSystemBootClasspathProjectIsProject() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    Project project = new Project();
    javaConstantResource.setClasspath(new Path(project));
    Path classpath = Path.systemBootClasspath;
    classpath.setProject(null);

    // Act
    javaConstantResource.setClasspath(classpath);

    // Assert
    assertSame(project, classpath.getProject());
  }

  /**
   * Test {@link AbstractClasspathResource#createClasspath()}.
   * <ul>
   *   <li>Then {@link JavaConstantResource} (default constructor) Classpath Description is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenJavaConstantResourceClasspathDescriptionIsNull() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();

    // Act
    Path actualCreateClasspathResult = javaConstantResource.createClasspath();

    // Assert
    Path classpath = javaConstantResource.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(actualCreateClasspathResult.getProject());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertEquals(0, classpath.size());
    assertFalse(classpath.isReference());
    assertTrue(classpath.isEmpty());
  }

  /**
   * Test {@link AbstractClasspathResource#createClasspath()}.
   * <ul>
   *   <li>Then {@link JavaConstantResource} (default constructor) Classpath is {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#createClasspath()}
   */
  @Test
  public void testCreateClasspath_thenJavaConstantResourceClasspathIsSystemBootClasspath() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    javaConstantResource.setClasspath(Path.systemBootClasspath);

    // Act and Assert
    Path expectedClasspath = javaConstantResource.createClasspath().systemBootClasspath;
    assertSame(expectedClasspath, javaConstantResource.getClasspath());
  }

  /**
   * Test {@link AbstractClasspathResource#setClasspathRef(Reference)}.
   * <ul>
   *   <li>Then {@link JavaConstantResource} (default constructor) Classpath Project is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#setClasspathRef(Reference)}
   */
  @Test
  public void testSetClasspathRef_thenJavaConstantResourceClasspathProjectIsNull() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();

    // Act
    javaConstantResource.setClasspathRef(new Reference("42"));

    // Assert
    Path classpath = javaConstantResource.getClasspath();
    assertNull(classpath.getDescription());
    assertNull(classpath.getProject());
    assertNull(classpath.getRefid());
    assertFalse(classpath.isReference());
  }

  /**
   * Test {@link AbstractClasspathResource#getClasspath()}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#getClasspath()}
   */
  @Test
  public void testGetClasspath_givenJavaConstantResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new JavaConstantResource()).getClasspath());
  }

  /**
   * Test {@link AbstractClasspathResource#getClasspath()}.
   * <ul>
   *   <li>Then return {@link Path#systemBootClasspath}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#getClasspath()}
   */
  @Test
  public void testGetClasspath_thenReturnSystemBootClasspath() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    javaConstantResource.setClasspath(Path.systemBootClasspath);

    // Act
    Path actualClasspath = javaConstantResource.getClasspath();

    // Assert
    assertSame(actualClasspath.systemBootClasspath, actualClasspath);
  }

  /**
   * Test {@link AbstractClasspathResource#getLoader()}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#getLoader()}
   */
  @Test
  public void testGetLoader_givenJavaConstantResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new JavaConstantResource()).getLoader());
  }

  /**
   * Test {@link AbstractClasspathResource#setLoaderRef(Reference)}.
   * <p>
   * Method under test: {@link AbstractClasspathResource#setLoaderRef(Reference)}
   */
  @Test
  public void testSetLoaderRef() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    Reference r = new Reference("42");

    // Act
    javaConstantResource.setLoaderRef(r);

    // Assert
    assertSame(r, javaConstantResource.getLoader());
  }

  /**
   * Test {@link AbstractClasspathResource#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then {@link JavaConstantResource} (default constructor) Reference.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenJavaConstantResource_thenJavaConstantResourceReference() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    Reference r = new Reference("42");

    // Act
    javaConstantResource.setRefid(r);

    // Assert
    assertTrue(javaConstantResource.isReference());
    assertSame(r, javaConstantResource.getRefid());
  }

  /**
   * Test {@link AbstractClasspathResource#isExists()}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor) Classpath is {@code null}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#isExists()}
   */
  @Test
  public void testIsExists_givenJavaConstantResourceClasspathIsNull_thenReturnFalse() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    javaConstantResource.setLoaderRef(null);
    javaConstantResource.setClasspath(null);
    javaConstantResource.setParentFirst(false);

    // Act and Assert
    assertFalse(javaConstantResource.isExists());
  }

  /**
   * Test {@link AbstractClasspathResource#isExists()}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#isExists()}
   */
  @Test
  public void testIsExists_givenJavaConstantResource_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new JavaConstantResource()).isExists());
  }

  /**
   * Test {@link AbstractClasspathResource#getClassLoader()}.
   * <ul>
   *   <li>Then return Loader is not {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractClasspathResource#getClassLoader()}
   */
  @Test
  public void testGetClassLoader_thenReturnLoaderIsNotNull() {
    // Arrange
    JavaConstantResource javaConstantResource = new JavaConstantResource();
    javaConstantResource.setProject(new Project());
    javaConstantResource.setLoaderRef(null);
    javaConstantResource.setClasspath(new Path(new Project()));
    javaConstantResource.setParentFirst(false);

    // Act and Assert
    assertNotNull(javaConstantResource.getClassLoader().getLoader());
  }
}
