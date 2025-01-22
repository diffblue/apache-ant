package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DefaultLogger;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.FileList;
import org.apache.tools.ant.types.FileList.FileName;
import org.apache.tools.ant.types.Path;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.ResourceCollection;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.types.resources.Resources;
import org.junit.Test;

public class UnpackDiffblueTest {
  /**
   * Test {@link Unpack#setSrc(File)} with {@code File}.
   * <ul>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(File)}
   */
  @Test
  public void testSetSrcWithFile_whenNull_file_placeholder_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BUnzip2()).setSrc(Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Unpack#setSrc(File)} with {@code File}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(File)}
   */
  @Test
  public void testSetSrcWithFile_whenPropertyIsJavaIoTmpdirIsDotDotToFile() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile()));
  }

  /**
   * Test {@link Unpack#setSrc(File)} with {@code File}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(File)}
   */
  @Test
  public void testSetSrcWithFile_whenPropertyIsJavaIoTmpdirIsDotToFile_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()));
  }

  /**
   * Test {@link Unpack#setSrc(File)} with {@code File}.
   * <ul>
   *   <li>When Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(File)}
   */
  @Test
  public void testSetSrcWithFile_whenPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrc(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrc("DEPRECATED - The setSrc(String) method has been deprecated. Use setSrc(File) instead."));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Src}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_givenJavaLangObject_whenSrc_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(".", typeClass);
    project.addBuildListener(new AntClassLoader());

    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc("Src"));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   *   <li>When {@code Src}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_givenProjectAddBuildListenerAntClassLoader_whenSrc() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc("Src"));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   *   <li>When {@code Src}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_givenProjectAddBuildListenerDefaultLogger_whenSrc() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(project);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc("Src"));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>When {@code ..}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_whenDotDot_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc(".."));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_whenDot_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc("."));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_whenEmptyString_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc(""));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>When Property is {@code user.dir}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_whenPropertyIsUserDir_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc(System.getProperty("user.dir")));
  }

  /**
   * Test {@link Unpack#setSrc(String)} with {@code String}.
   * <ul>
   *   <li>When {@code Src}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrc(String)}
   */
  @Test
  public void testSetSrcWithString_whenSrc_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrc("Src"));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrcResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, Manifest.ATTRIBUTE_NAME)));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileResource src = new FileResource();
    src.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(src));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenFileAttributeIsNull() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileResource src = new FileResource();
    src.setName("file attribute is null!");

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(src));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenNull_file_placeholder() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(src));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenNull_file_placeholder2() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileResource src = new FileResource();
    src.setBaseDir(Copy.NULL_FILE_PLACEHOLDER);
    src.setName(System.getProperty("user.dir"));

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(src));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given Property is {@code user.dir}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenPropertyIsUserDir() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileResource src = new FileResource();
    src.setName(System.getProperty("user.dir"));

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(src));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>Given {@code true}.</li>
   *   <li>When {@link Resource#Resource()} Exists is {@code true}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_givenTrue_whenResourceExistsIsTrue_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Resource src = new Resource();
    src.setDirectory(true);
    src.setExists(true);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(src));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File, String)} with b is {@code null} and name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenFileResourceWithBIsNullAndNameIsAttribute_name() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrcResource(new FileResource((File) null, Manifest.ATTRIBUTE_NAME)));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File, String)} with b is {@link Copy#NULL_FILE_PLACEHOLDER} and name is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenFileResourceWithBIsNull_file_placeholderAndNameIsDot() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, ".")));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File, String)} with b is {@link Copy#NULL_FILE_PLACEHOLDER} and name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenFileResourceWithBIsNull_file_placeholderAndNameIsDotDot() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrcResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, "..")));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenFileResourceWithFIsNull_file_placeholder() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER)));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotDotToFile() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile())));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotToFile() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> bUnzip2.setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile())));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2
        .setSrcResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenJavaConstantResource_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(new JavaConstantResource()));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link FileResource#FileResource(File)} and m is {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenMappedResourceWithRIsFileResourceAndMIsFilterMapper() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    FileResource r = new FileResource(Copy.NULL_FILE_PLACEHOLDER);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(new MappedResource(r, new FilterMapper())));
  }

  /**
   * Test {@link Unpack#setSrcResource(Resource)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setSrcResource(Resource)}
   */
  @Test
  public void testSetSrcResource_whenResourceWithNameIsAttribute_name_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.setSrcResource(new Resource(Manifest.ATTRIBUTE_NAME)));
  }

  /**
   * Test {@link Unpack#setDest(File)} with {@code File}.
   * <p>
   * Method under test: {@link Unpack#setDest(File)}
   */
  @Test
  public void testSetDestWithFile() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act
    bUnzip2.setDest(Copy.NULL_FILE_PLACEHOLDER);

    // Assert
    File file = bUnzip2.dest;
    assertEquals("NULL_FILE", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#setDest(String)} with {@code String}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>When {@code Dest}.</li>
   *   <li>Then {@link BUnzip2} (default constructor) {@link Unpack#dest} Name is {@code Dest}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setDest(String)}
   */
  @Test
  public void testSetDestWithString_givenJavaLangObject_whenDest_thenBUnzip2DestNameIsDest() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition(".", typeClass);
    project.addBuildListener(new AntClassLoader());

    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(project);

    // Act
    bUnzip2.setDest("Dest");

    // Assert
    File file = bUnzip2.dest;
    assertEquals("Dest", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#setDest(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setDest(String)}
   */
  @Test
  public void testSetDestWithString_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(project);

    // Act
    bUnzip2.setDest("Dest");

    // Assert
    File file = bUnzip2.dest;
    assertEquals("Dest", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#setDest(String)} with {@code String}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link DefaultLogger} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setDest(String)}
   */
  @Test
  public void testSetDestWithString_givenProjectAddBuildListenerDefaultLogger() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new DefaultLogger());

    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(project);

    // Act
    bUnzip2.setDest("Dest");

    // Assert
    File file = bUnzip2.dest;
    assertEquals("Dest", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#setDest(String)} with {@code String}.
   * <ul>
   *   <li>When {@code Dest}.</li>
   *   <li>Then {@link BUnzip2} (default constructor) {@link Unpack#dest} Name is {@code Dest}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setDest(String)}
   */
  @Test
  public void testSetDestWithString_whenDest_thenBUnzip2DestNameIsDest() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act
    bUnzip2.setDest("Dest");

    // Assert
    File file = bUnzip2.dest;
    assertEquals("Dest", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#setDest(String)} with {@code String}.
   * <ul>
   *   <li>When {@code ..}.</li>
   *   <li>Then {@link BUnzip2} (default constructor) {@link Unpack#dest} Name is {@code Downloads}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setDest(String)}
   */
  @Test
  public void testSetDestWithString_whenDotDot_thenBUnzip2DestNameIsDownloads() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act
    bUnzip2.setDest("..");

    // Assert
    File file = bUnzip2.dest;
    assertEquals("Downloads", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#setDest(String)} with {@code String}.
   * <ul>
   *   <li>When {@code .}.</li>
   *   <li>Then {@link BUnzip2} (default constructor) {@link Unpack#dest} Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setDest(String)}
   */
  @Test
  public void testSetDestWithString_whenDot_thenBUnzip2DestNameIsApacheAnt11015() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act
    bUnzip2.setDest(".");

    // Assert
    File file = bUnzip2.dest;
    assertEquals("apache-ant-1.10.15", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#setDest(String)} with {@code String}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then {@link BUnzip2} (default constructor) {@link Unpack#dest} Name is {@code apache-ant-1.10.15}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#setDest(String)}
   */
  @Test
  public void testSetDestWithString_whenEmptyString_thenBUnzip2DestNameIsApacheAnt11015() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.setProject(new Project());

    // Act
    bUnzip2.setDest("");

    // Assert
    File file = bUnzip2.dest;
    assertEquals("apache-ant-1.10.15", file.getName());
    assertTrue(file.isAbsolute());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Concat a = new Concat();
    a.addText("At least one resource must be provided, or some text.");

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileList#FileList()} addConfiguredFile {@link FileName} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileListAddConfiguredFileFileName() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName(Manifest.ATTRIBUTE_NAME);

    FileList list = new FileList();
    list.addConfiguredFile(name);

    Concat a = new Concat();
    a.addFilelist(list);

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsAttribute_name() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName(Manifest.ATTRIBUTE_NAME);

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(a));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsAttribute_name2() throws BuildException {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName(Manifest.ATTRIBUTE_NAME);

    FileList a = new FileList();
    a.setDir(Copy.NULL_FILE_PLACEHOLDER);
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(a));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDotDot() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName("..");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(a));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDotDot2() throws BuildException {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName("..");

    FileList a = new FileList();
    a.setDir(Copy.NULL_FILE_PLACEHOLDER);
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(a));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code .}.</li>
   *   <li>When {@link FileList#FileList()} addConfiguredFile {@link FileName} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDot_whenFileListAddConfiguredFileFileName() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName(".");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(a));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is {@code .}.</li>
   *   <li>When {@link FileList#FileList()} Dir is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsDot_whenFileListDirIsNull_file_placeholder() throws BuildException {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName(".");

    FileList a = new FileList();
    a.setDir(Copy.NULL_FILE_PLACEHOLDER);
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(a));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileName} (default constructor) Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileNameNameIsEmptyString() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    FileName name = new FileName();
    name.setName("");

    FileList a = new FileList();
    a.addConfiguredFile(name);

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(a));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link FileResource#FileResource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenFileResource_whenConcatDestIsFileResource() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Concat a = new Concat();
    a.setDest(new FileResource());
    a.addFilelist(new FileList());

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Concat a = new Concat();
    a.setProject(project);
    a.addText("At least one resource must be provided, or some text.");

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor).</li>
   *   <li>When {@link Concat} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenProject_whenConcatProjectIsProject() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Concat a = new Concat();
    a.setProject(new Project());
    a.addText("At least one resource must be provided, or some text.");

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat (At least one resource must be provided, or some text.)\"",
        resource.toLongString());
    assertEquals("concat (At least one resource must be provided, or some text.)", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with name is {@code No directory specified for %s.}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResourceWithNameIsNoDirectorySpecifiedForS() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Concat a = new Concat();
    a.setDest(new Resource("No directory specified for %s."));
    a.addFilelist(new FileList());

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Concat} (default constructor) Dest is {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_givenResource_whenConcatDestIsResource() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Concat a = new Concat();
    a.setDest(new Resource());
    a.addFilelist(new FileList());

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>Then {@link BUnzip2} (default constructor) {@link Unpack#srcResource} toLongString is {@code Concat$ConcatResource "concat ()"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_thenBUnzip2SrcResourceToLongStringIsConcatConcatResourceConcat() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    Concat a = new Concat();
    a.addFilelist(new FileList());

    // Act
    bUnzip2.addConfigured(a);

    // Assert
    Resource resource = bUnzip2.srcResource;
    assertEquals("Concat$ConcatResource \"concat ()\"", resource.toLongString());
    assertEquals("concat ()", resource.getName());
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link FileList#FileList()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenFileList_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(new FileList()));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resources#NONE}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenNone_thenThrowBuildException() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BUnzip2()).addConfigured(Resources.NONE));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Path#Path(Project, String)} with p is {@link Project} (default constructor) and {@code Path}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenPathWithPIsProjectAndPath_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(new Path(new Project(), "Path")));
  }

  /**
   * Test {@link Unpack#addConfigured(ResourceCollection)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#addConfigured(ResourceCollection)}
   */
  @Test
  public void testAddConfigured_whenResourceWithNameIsAttribute_name_thenThrowBuildException() {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.addConfigured(new Resource(Manifest.ATTRIBUTE_NAME)));
  }

  /**
   * Test {@link Unpack#execute()}.
   * <ul>
   *   <li>Given {@link BUnzip2} (default constructor) addConfigured {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#execute()}
   */
  @Test
  public void testExecute_givenBUnzip2AddConfiguredResource_thenThrowBuildException() throws BuildException {
    // Arrange
    BUnzip2 bUnzip2 = new BUnzip2();
    bUnzip2.addConfigured(new Resource());

    // Act and Assert
    assertThrows(BuildException.class, () -> bUnzip2.execute());
  }

  /**
   * Test {@link Unpack#execute()}.
   * <ul>
   *   <li>Given {@link BUnzip2} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Unpack#execute()}
   */
  @Test
  public void testExecute_givenBUnzip2_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new BUnzip2()).execute());
  }

  /**
   * Test {@link Unpack#supportsNonFileResources()}.
   * <p>
   * Method under test: {@link Unpack#supportsNonFileResources()}
   */
  @Test
  public void testSupportsNonFileResources() {
    // Arrange, Act and Assert
    assertTrue((new BUnzip2()).supportsNonFileResources());
  }
}
