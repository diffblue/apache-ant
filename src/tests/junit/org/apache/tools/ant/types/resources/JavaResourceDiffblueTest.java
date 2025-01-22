package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Paths;
import javax.management.loading.MLet;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class JavaResourceDiffblueTest {
  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    // Act and Assert
    assertThrows(FileNotFoundException.class,
        () -> javaResource.openInputStream(new AntClassLoader(new AntClassLoader(), true)));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream2() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    AntClassLoader cl = new AntClassLoader();
    cl.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "Couldn't load ResourceStream for ").toFile());

    // Act and Assert
    assertThrows(FileNotFoundException.class, () -> javaResource.openInputStream(cl));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream3() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    // Act and Assert
    assertThrows(FileNotFoundException.class,
        () -> javaResource.openInputStream(new AntClassLoader(new AntClassLoader(), false)));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream4() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Resource ", Path.systemBootClasspath);

    // Act and Assert
    assertThrows(FileNotFoundException.class,
        () -> javaResource.openInputStream(
            new URLClassLoader(new URL[]{Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toUri().toURL()},
                new AntClassLoader())));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>Given {@code .}.</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} addPathElement {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_givenDot_whenAntClassLoaderAddPathElementDot() throws IOException, BuildException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    AntClassLoader cl = new AntClassLoader();
    cl.addPathElement(".");
    cl.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(FileNotFoundException.class, () -> javaResource.openInputStream(cl));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_givenJavaLangObject_whenAntClassLoaderProjectIsProject() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("Couldn't load ResourceStream for ", typeClass);

    AntClassLoader cl = new AntClassLoader();
    cl.setProject(project);
    cl.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(FileNotFoundException.class, () -> javaResource.openInputStream(cl));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link AntClassLoader#AntClassLoader()} Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_givenProject_whenAntClassLoaderProjectIsProject() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    AntClassLoader cl = new AntClassLoader();
    cl.setProject(new Project());
    cl.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(FileNotFoundException.class, () -> javaResource.openInputStream(cl));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>Given Property is {@code java.io.tmpdir} is array of {@link String} with {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_givenPropertyIsJavaIoTmpdirIsArrayOfStringWithTestTxtToFile() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    AntClassLoader cl = new AntClassLoader();
    cl.addPathComponent(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(FileNotFoundException.class, () -> javaResource.openInputStream(cl));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>When {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_whenAntClassLoader() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    // Act and Assert
    assertThrows(FileNotFoundException.class, () -> javaResource.openInputStream(new AntClassLoader()));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>When {@link MLet#MLet()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_whenMLet() throws IOException {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    // Act and Assert
    assertThrows(FileNotFoundException.class, () -> javaResource.openInputStream(new MLet()));
  }

  /**
   * Test {@link JavaResource#openInputStream(ClassLoader)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#openInputStream(ClassLoader)}
   */
  @Test
  public void testOpenInputStream_whenNull() throws IOException {
    // Arrange, Act and Assert
    assertThrows(FileNotFoundException.class,
        () -> (new JavaResource("Name", Path.systemBootClasspath)).openInputStream(null));
  }

  /**
   * Test {@link JavaResource#getURL()}.
   * <ul>
   *   <li>Given {@link JavaResource#JavaResource()} Name is {@code Name}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#getURL()}
   */
  @Test
  public void testGetURL_givenJavaResourceNameIsName_thenReturnNull() {
    // Arrange
    JavaResource javaResource = new JavaResource();
    javaResource.setName("Name");

    // Act and Assert
    assertNull(javaResource.getURL());
  }

  /**
   * Test {@link JavaResource#getURL()}.
   * <ul>
   *   <li>Given {@link JavaResource#JavaResource(String, Path)} with {@code Name} and path is {@link Path#Path(Project)} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#getURL()}
   */
  @Test
  public void testGetURL_givenJavaResourceWithNameAndPathIsPathProjectIsProject_thenReturnNull() {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", new Path(new Project()));
    javaResource.setProject(new Project());

    // Act and Assert
    assertNull(javaResource.getURL());
  }

  /**
   * Test {@link JavaResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Given {@code Name}.</li>
   *   <li>When {@link JavaResource#JavaResource()} Name is {@code Name}.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_givenName_whenJavaResourceNameIsName_thenReturnOne() {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    JavaResource another = new JavaResource();
    another.setName("Name");

    // Act and Assert
    assertEquals(1, javaResource.compareTo(another));
  }

  /**
   * Test {@link JavaResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnMinusOne() {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", null);

    // Act and Assert
    assertEquals(-1, javaResource.compareTo(new JavaResource("Name", Path.systemBootClasspath)));
  }

  /**
   * Test {@link JavaResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return minus thirty-eight.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnMinusThirtyEight() {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    // Act and Assert
    assertEquals(-38,
        javaResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link JavaResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return thirty-one.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnThirtyOne() {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    // Act and Assert
    assertEquals(31,
        javaResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())));
  }

  /**
   * Test {@link JavaResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return thirty-three.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnThirtyThree() {
    // Arrange
    JavaResource javaResource = new JavaResource("org.apache.tools.ant.types.resources.JavaResource",
        Path.systemBootClasspath);

    // Act and Assert
    assertEquals(33, javaResource.compareTo(new JavaResource("Name", Path.systemBootClasspath)));
  }

  /**
   * Test {@link JavaResource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return zero.</li>
   * </ul>
   * <p>
   * Method under test: {@link JavaResource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnZero() {
    // Arrange
    JavaResource javaResource = new JavaResource("Name", Path.systemBootClasspath);

    // Act and Assert
    assertEquals(0, javaResource.compareTo(new JavaResource("Name", Path.systemBootClasspath)));
  }
}
