package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.ByteArrayInputStream;
import java.io.File;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Paths;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.taskdefs.Untar.UntarCompressionMethod;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.mappers.FilterMapper;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.util.FileUtils;
import org.apache.tools.ant.util.IdentityMapper;
import org.junit.Test;

public class UntarDiffblueTest {
  /**
   * Test new {@link Untar} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Untar}
   */
  @Test
  public void testNewUntar() {
    // Arrange and Act
    Untar actualUntar = new Untar();

    // Assert
    assertTrue(actualUntar.getMapper() instanceof IdentityMapper);
    assertNull(actualUntar.getAllowFilesToEscapeDest());
    assertNull(actualUntar.getDescription());
    assertNull(actualUntar.getTaskName());
    assertNull(actualUntar.getTaskType());
    assertNull(actualUntar.getEncoding());
    assertNull(actualUntar.getProject());
    assertNull(actualUntar.getOwningTarget());
    assertFalse(actualUntar.getFailOnEmptyArchive());
    assertTrue(actualUntar.getScanForUnicodeExtraFields());
  }

  /**
   * Test {@link Untar#setScanForUnicodeExtraFields(boolean)}.
   * <p>
   * Method under test: {@link Untar#setScanForUnicodeExtraFields(boolean)}
   */
  @Test
  public void testSetScanForUnicodeExtraFields() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Untar()).setScanForUnicodeExtraFields(true));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenJavaLangObject_thenThrowBuildException() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("none", typeClass);
    project.addBuildListener(new AntClassLoader());

    Untar untar = new Untar();
    untar.setProject(project);
    untar.add(new CutDirsMapper());
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Project} (default constructor) addBuildListener {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenProjectAddBuildListenerAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    Untar untar = new Untar();
    untar.setProject(project);
    untar.add(new CutDirsMapper());
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenUntarAddCutDirsMapper_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();
    untar.add(new CutDirsMapper());
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) internalSetEncoding {@code gzip}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenUntarInternalSetEncodingGzip_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();
    untar.internalSetEncoding("gzip");
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) internalSetEncoding {@link Manifest#JAR_ENCODING}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenUntarInternalSetEncodingJar_encoding_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();
    untar.internalSetEncoding(Manifest.JAR_ENCODING);
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) internalSetEncoding {@code UTF8}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenUntarInternalSetEncodingUtf8_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();
    untar.internalSetEncoding("UTF8");
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) Project is {@link Project} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenUntarProjectIsProject_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();
    untar.setProject(new Project());
    untar.add(new CutDirsMapper());
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenUntar_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();
    FileUtils fileUtils = FileUtils.getFileUtils();

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandFile(fileUtils,
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandFile(FileUtils, File, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor).</li>
   *   <li>When {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandFile(FileUtils, File, File)}
   */
  @Test
  public void testExpandFile_givenUntar_whenNull_file_placeholder_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandFile(FileUtils.getFileUtils(), Copy.NULL_FILE_PLACEHOLDER, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, Manifest.ATTRIBUTE_NAME),
            Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link FileResource#FileResource()} Name is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_givenEmptyString_whenFileResourceNameIsEmptyString() {
    // Arrange
    Untar untar = new Untar();

    FileResource srcR = new FileResource();
    srcR.setName("");

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandResource(srcR, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_givenFileAttributeIsNull() {
    // Arrange
    Untar untar = new Untar();

    FileResource srcR = new FileResource();
    srcR.setName("file attribute is null!");

    // Act and Assert
    assertThrows(BuildException.class, () -> untar.expandResource(srcR, Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) add {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_givenUntarAddCutDirsMapper() {
    // Arrange
    Untar untar = new Untar();
    untar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(
            new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()),
            Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) internalSetEncoding {@link Manifest#JAR_ENCODING}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_givenUntarInternalSetEncodingJar_encoding() {
    // Arrange
    Untar untar = new Untar();
    untar.internalSetEncoding(Manifest.JAR_ENCODING);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(
            new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()),
            Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor) Project is {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_givenUntarProjectIsProject() {
    // Arrange
    Untar untar = new Untar();
    untar.setProject(new Project());
    untar.add(new CutDirsMapper());

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(
            new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()),
            Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>Given {@link Untar} (default constructor).</li>
   *   <li>When {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_givenUntar_whenJavaConstantResource_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new JavaConstantResource(), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File, String)} with b is {@code null} and name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenFileResourceWithBIsNullAndNameIsAttribute_name() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new FileResource((File) null, Manifest.ATTRIBUTE_NAME), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File, String)} with b is {@link Copy#NULL_FILE_PLACEHOLDER} and name is {@code .}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenFileResourceWithBIsNull_file_placeholderAndNameIsDot() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, "."), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File, String)} with b is {@link Copy#NULL_FILE_PLACEHOLDER} and name is {@code ..}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenFileResourceWithBIsNull_file_placeholderAndNameIsDotDot() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER, ".."), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is {@link Copy#NULL_FILE_PLACEHOLDER}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenFileResourceWithFIsNull_file_placeholder() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new FileResource(Copy.NULL_FILE_PLACEHOLDER), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code ..} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotDotToFile() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "..").toFile()),
            Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code .} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsDotToFile() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), ".").toFile()),
            Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link FileResource#FileResource(File)} with f is Property is {@code java.io.tmpdir} is {@code test.txt} toFile.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenFileResourceWithFIsPropertyIsJavaIoTmpdirIsTestTxtToFile() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(
            new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()),
            Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource(String)} and m is {@link FilterMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenMappedResourceWithRIsResourceAndMIsFilterMapper() {
    // Arrange
    Untar untar = new Untar();
    Resource r = new Resource(Manifest.ATTRIBUTE_NAME);

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new MappedResource(r, new FilterMapper()), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test {@link Untar#expandResource(Resource, File)}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with name is {@link Manifest#ATTRIBUTE_NAME}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Untar#expandResource(Resource, File)}
   */
  @Test
  public void testExpandResource_whenResourceWithNameIsAttribute_name_thenThrowBuildException() {
    // Arrange
    Untar untar = new Untar();

    // Act and Assert
    assertThrows(BuildException.class,
        () -> untar.expandResource(new Resource(Manifest.ATTRIBUTE_NAME), Copy.NULL_FILE_PLACEHOLDER));
  }

  /**
   * Test UntarCompressionMethod {@link UntarCompressionMethod#decompress(String, InputStream)}.
   * <p>
   * Method under test: {@link UntarCompressionMethod#decompress(String, InputStream)}
   */
  @Test
  public void testUntarCompressionMethodDecompress() throws IOException, BuildException {
    // Arrange
    ByteArrayInputStream istream = new ByteArrayInputStream("AXAXAXAX".getBytes("UTF-8"));

    // Act and Assert
    byte[] byteArray = new byte[8];
    int actualReadResult = (new UntarCompressionMethod()).decompress(Manifest.ATTRIBUTE_NAME, istream).read(byteArray);
    assertEquals(-1, istream.read(new byte[]{}));
    assertEquals(8, actualReadResult);
    assertArrayEquals("AXAXAXAX".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test UntarCompressionMethod {@link UntarCompressionMethod#getValues()}.
   * <p>
   * Method under test: {@link UntarCompressionMethod#getValues()}
   */
  @Test
  public void testUntarCompressionMethodGetValues() {
    // Arrange, Act and Assert
    assertArrayEquals(new String[]{"none", "gzip", "bzip2", "xz"}, (new UntarCompressionMethod()).getValues());
  }

  /**
   * Test UntarCompressionMethod new {@link UntarCompressionMethod} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link UntarCompressionMethod}
   */
  @Test
  public void testUntarCompressionMethodNewUntarCompressionMethod() {
    // Arrange and Act
    UntarCompressionMethod actualUntarCompressionMethod = new UntarCompressionMethod();

    // Assert
    assertEquals("none", actualUntarCompressionMethod.getValue());
    assertEquals(0, actualUntarCompressionMethod.getIndex());
    assertArrayEquals(new String[]{"none", "gzip", "bzip2", "xz"}, actualUntarCompressionMethod.getValues());
  }
}
