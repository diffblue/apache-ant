package org.apache.tools.ant.types;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.nio.file.Paths;
import java.util.Iterator;
import java.util.Optional;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.types.mappers.CutDirsMapper;
import org.apache.tools.ant.types.resources.BZip2Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.apache.tools.ant.types.resources.JavaConstantResource;
import org.apache.tools.ant.types.resources.MappedResource;
import org.apache.tools.ant.types.resources.PropertyResource;
import org.apache.tools.ant.types.resources.StringResource;
import org.junit.Test;

public class ResourceDiffblueTest {
  /**
   * Test {@link Resource#getMagicNumber(byte[])}.
   * <ul>
   *   <li>When {@code AXAXAXAX} Bytes is {@code UTF-8}.</li>
   *   <li>Then return {@code 1096302936}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getMagicNumber(byte[])}
   */
  @Test
  public void testGetMagicNumber_whenAxaxaxaxBytesIsUtf8_thenReturn1096302936() throws UnsupportedEncodingException {
    // Arrange, Act and Assert
    assertEquals(1096302936, Resource.getMagicNumber("AXAXAXAX".getBytes("UTF-8")));
  }

  /**
   * Test {@link Resource#getName()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getName()}
   */
  @Test
  public void testGetName_givenResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertNull((new Resource()).getName());
  }

  /**
   * Test {@link Resource#getName()}.
   * <ul>
   *   <li>Then return {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getName()}
   */
  @Test
  public void testGetName_thenReturnFileAttributeIsNull() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals("file attribute is null!", fileResource.getName());
  }

  /**
   * Test {@link Resource#setName(String)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then {@link Resource#Resource()} Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setName(String)}
   */
  @Test
  public void testSetName_givenResource_thenResourceNameIsName() {
    // Arrange
    Resource resource = new Resource();

    // Act
    resource.setName("Name");

    // Assert
    assertEquals("Name", resource.getName());
    assertEquals("Resource \"Name\"", resource.toLongString());
  }

  /**
   * Test {@link Resource#setName(String)}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()}.</li>
   *   <li>Then {@link StringResource#StringResource()} Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setName(String)}
   */
  @Test
  public void testSetName_givenStringResource_thenStringResourceNameIsName() throws IOException {
    // Arrange
    StringResource stringResource = new StringResource();

    // Act
    stringResource.setName("Name");

    // Assert
    assertEquals("Name", stringResource.getName());
    assertEquals("Name", stringResource.getValue());
    assertEquals("StringResource \"Name\"", stringResource.toLongString());
    byte[] byteArray = new byte[4];
    assertEquals(4, stringResource.getInputStream().read(byteArray));
    assertEquals(4L, stringResource.getSize());
    assertTrue(stringResource.isExists());
    assertArrayEquals("Name".getBytes("UTF-8"), byteArray);
  }

  /**
   * Test {@link Resource#isExists()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isExists()}
   */
  @Test
  public void testIsExists_givenFileResourceNameIsFileAttributeIsNull_thenReturnFalse() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertFalse(fileResource.isExists());
  }

  /**
   * Test {@link Resource#isExists()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isExists()}
   */
  @Test
  public void testIsExists_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertTrue((new MappedResource(r, new CutDirsMapper())).isExists());
  }

  /**
   * Test {@link Resource#isExists()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} Exists is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isExists()}
   */
  @Test
  public void testIsExists_givenResourceExistsIsTrue_thenReturnTrue() {
    // Arrange
    Resource resource = new Resource();
    resource.setExists(true);

    // Act and Assert
    assertTrue(resource.isExists());
  }

  /**
   * Test {@link Resource#isExists()}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isExists()}
   */
  @Test
  public void testIsExists_givenResourceWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Resource("Name")).isExists());
  }

  /**
   * Test {@link Resource#isExists()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isExists()}
   */
  @Test
  public void testIsExists_givenResource_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Resource()).isExists());
  }

  /**
   * Test {@link Resource#setExists(boolean)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@code false}.</li>
   *   <li>Then {@link Resource#Resource()} Size is {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setExists(boolean)}
   */
  @Test
  public void testSetExists_givenResource_whenFalse_thenResourceSizeIsUnknown_datetime() {
    // Arrange
    Resource resource = new Resource();

    // Act
    resource.setExists(false);

    // Assert
    assertEquals(Resource.UNKNOWN_DATETIME, resource.getSize());
  }

  /**
   * Test {@link Resource#setExists(boolean)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@code true}.</li>
   *   <li>Then {@link Resource#Resource()} Size is {@link Resource#UNKNOWN_SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setExists(boolean)}
   */
  @Test
  public void testSetExists_givenResource_whenTrue_thenResourceSizeIsUnknown_size() {
    // Arrange
    Resource resource = new Resource();

    // Act
    resource.setExists(true);

    // Assert that nothing has changed
    assertEquals(Resource.UNKNOWN_SIZE, resource.getSize());
  }

  /**
   * Test {@link Resource#getLastModified()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenFileResourceNameIsFileAttributeIsNull() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, fileResource.getLastModified());
  }

  /**
   * Test {@link Resource#getLastModified()}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenJavaConstantResource_thenReturnUnknown_datetime() {
    // Arrange, Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, (new JavaConstantResource()).getLastModified());
  }

  /**
   * Test {@link Resource#getLastModified()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, (new MappedResource(r, new CutDirsMapper())).getLastModified());
  }

  /**
   * Test {@link Resource#getLastModified()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} LastModified is {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenResourceLastModifiedIsUnknown_datetime() {
    // Arrange
    Resource resource = new Resource();
    resource.setLastModified(Resource.UNKNOWN_DATETIME);
    resource.setExists(true);

    // Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, resource.getLastModified());
  }

  /**
   * Test {@link Resource#getLastModified()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} LastModified is {@link Resource#UNKNOWN_SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenResourceLastModifiedIsUnknown_size() {
    // Arrange
    Resource resource = new Resource();
    resource.setLastModified(Resource.UNKNOWN_SIZE);
    resource.setExists(true);

    // Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, resource.getLastModified());
  }

  /**
   * Test {@link Resource#getLastModified()}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenResourceWithName_thenReturnUnknown_datetime() {
    // Arrange, Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, (new Resource("Name")).getLastModified());
  }

  /**
   * Test {@link Resource#getLastModified()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getLastModified()}
   */
  @Test
  public void testGetLastModified_givenResource_thenReturnUnknown_datetime() {
    // Arrange, Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, (new Resource()).getLastModified());
  }

  /**
   * Test {@link Resource#setLastModified(long)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then {@link Resource#Resource()} LastModified is one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setLastModified(long)}
   */
  @Test
  public void testSetLastModified_givenResource_thenResourceLastModifiedIsOne() {
    // Arrange
    Resource resource = new Resource();

    // Act
    resource.setLastModified(1L);

    // Assert
    assertEquals(1L, resource.getLastModified());
  }

  /**
   * Test {@link Resource#isDirectory()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenFileResourceNameIsFileAttributeIsNull_thenReturnFalse() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertFalse(fileResource.isDirectory());
  }

  /**
   * Test {@link Resource#isDirectory()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertFalse((new MappedResource(r, new CutDirsMapper())).isDirectory());
  }

  /**
   * Test {@link Resource#isDirectory()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} Directory is {@code true}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenResourceDirectoryIsTrue_thenReturnTrue() {
    // Arrange
    Resource resource = new Resource();
    resource.setDirectory(true);

    // Act and Assert
    assertTrue(resource.isDirectory());
  }

  /**
   * Test {@link Resource#isDirectory()}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenResourceWithName_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Resource("Name")).isDirectory());
  }

  /**
   * Test {@link Resource#isDirectory()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isDirectory()}
   */
  @Test
  public void testIsDirectory_givenResource_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Resource()).isDirectory());
  }

  /**
   * Test {@link Resource#setSize(long)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When three.</li>
   *   <li>Then {@link Resource#Resource()} Size is three.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setSize(long)}
   */
  @Test
  public void testSetSize_givenResource_whenThree_thenResourceSizeIsThree() {
    // Arrange
    Resource resource = new Resource();

    // Act
    resource.setSize(3L);

    // Assert
    assertEquals(3L, resource.getSize());
  }

  /**
   * Test {@link Resource#setSize(long)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@link Resource#UNKNOWN_SIZE}.</li>
   *   <li>Then {@link Resource#Resource()} Size is {@link Resource#UNKNOWN_SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setSize(long)}
   */
  @Test
  public void testSetSize_givenResource_whenUnknown_size_thenResourceSizeIsUnknown_size() {
    // Arrange
    Resource resource = new Resource();

    // Act
    resource.setSize(Resource.UNKNOWN_SIZE);

    // Assert that nothing has changed
    assertEquals(Resource.UNKNOWN_SIZE, resource.getSize());
  }

  /**
   * Test {@link Resource#getSize()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Name is {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getSize()}
   */
  @Test
  public void testGetSize_givenFileResourceNameIsFileAttributeIsNull() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, fileResource.getSize());
  }

  /**
   * Test {@link Resource#getSize()}.
   * <ul>
   *   <li>Given {@link JavaConstantResource} (default constructor).</li>
   *   <li>Then return {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getSize()}
   */
  @Test
  public void testGetSize_givenJavaConstantResource_thenReturnUnknown_datetime() {
    // Arrange, Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, (new JavaConstantResource()).getSize());
  }

  /**
   * Test {@link Resource#getSize()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getSize()}
   */
  @Test
  public void testGetSize_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertEquals(Resource.UNKNOWN_SIZE, (new MappedResource(r, new CutDirsMapper())).getSize());
  }

  /**
   * Test {@link Resource#getSize()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} Size is one.</li>
   *   <li>Then return one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getSize()}
   */
  @Test
  public void testGetSize_givenResourceSizeIsOne_thenReturnOne() {
    // Arrange
    Resource resource = new Resource();
    resource.setSize(1L);
    resource.setExists(true);

    // Act and Assert
    assertEquals(1L, resource.getSize());
  }

  /**
   * Test {@link Resource#getSize()}.
   * <ul>
   *   <li>Given {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@link Resource#UNKNOWN_DATETIME}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getSize()}
   */
  @Test
  public void testGetSize_givenResourceWithName_thenReturnUnknown_datetime() {
    // Arrange, Act and Assert
    assertEquals(Resource.UNKNOWN_DATETIME, (new Resource("Name")).getSize());
  }

  /**
   * Test {@link Resource#getSize()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@link Resource#UNKNOWN_SIZE}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getSize()}
   */
  @Test
  public void testGetSize_givenResource_thenReturnUnknown_size() {
    // Arrange, Act and Assert
    assertEquals(Resource.UNKNOWN_SIZE, (new Resource()).getSize());
  }

  /**
   * Test {@link Resource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>Then return minus one.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_thenReturnMinusOne() {
    // Arrange
    FileResource fileResource = new FileResource();

    // Act and Assert
    assertEquals(-1,
        fileResource.compareTo(new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile())));
  }

  /**
   * Test {@link Resource#compareTo(Resource)} with {@code Resource}.
   * <ul>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return twenty-four.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#compareTo(Resource)}
   */
  @Test
  public void testCompareToWithResource_whenResourceWithName_thenReturnTwentyFour() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("file attribute is null!");

    // Act and Assert
    assertEquals(24, fileResource.compareTo(new Resource("Name")));
  }

  /**
   * Test {@link Resource#equals(Object)}, and {@link Resource#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Resource#equals(Object)}
   *   <li>{@link Resource#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    Resource resource = new Resource();

    // Act and Assert
    assertEquals(resource, resource);
    int expectedHashCodeResult = resource.hashCode();
    assertEquals(expectedHashCodeResult, resource.hashCode());
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange
    BZip2Resource bZip2Resource = new BZip2Resource();

    // Act and Assert
    assertNotEquals(bZip2Resource, new Resource());
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual2() {
    // Arrange, Act and Assert
    assertNotEquals(new PropertyResource(new Project(), "foo"), null);
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Resource(), null);
  }

  /**
   * Test {@link Resource#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Resource(), "Different type to Resource");
  }

  /**
   * Test {@link Resource#getInputStream()}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper() throws IOException {
    // Arrange
    Resource r = new Resource();

    // Act and Assert
    assertThrows(UnsupportedOperationException.class,
        () -> (new MappedResource(r, new CutDirsMapper())).getInputStream());
  }

  /**
   * Test {@link Resource#getInputStream()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then throw {@link UnsupportedOperationException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#getInputStream()}
   */
  @Test
  public void testGetInputStream_givenResource_thenThrowUnsupportedOperationException() throws IOException {
    // Arrange, Act and Assert
    assertThrows(UnsupportedOperationException.class, () -> (new Resource()).getInputStream());
  }

  /**
   * Test {@link Resource#iterator()}.
   * <p>
   * Method under test: {@link Resource#iterator()}
   */
  @Test
  public void testIterator() {
    // Arrange
    Resource resource = new Resource();

    // Act
    Iterator<Resource> actualIteratorResult = resource.iterator();

    // Assert
    Resource actualNextResult = actualIteratorResult.next();
    assertFalse(actualIteratorResult.hasNext());
    assertSame(resource, actualNextResult);
  }

  /**
   * Test {@link Resource#size()}.
   * <p>
   * Method under test: {@link Resource#size()}
   */
  @Test
  public void testSize() {
    // Arrange, Act and Assert
    assertEquals(1, (new Resource()).size());
  }

  /**
   * Test {@link Resource#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()} Checked is {@code false}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileResourceCheckedIsFalse_thenReturnTrue() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setChecked(false);

    // Act and Assert
    assertTrue(fileResource.isFilesystemOnly());
  }

  /**
   * Test {@link Resource#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenFileResource_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new FileResource()).isFilesystemOnly());
  }

  /**
   * Test {@link Resource#isFilesystemOnly()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#isFilesystemOnly()}
   */
  @Test
  public void testIsFilesystemOnly_givenResource_thenReturnFalse() {
    // Arrange, Act and Assert
    assertFalse((new Resource()).isFilesystemOnly());
  }

  /**
   * Test {@link Resource#toString()}.
   * <ul>
   *   <li>Given {@link FileResource#FileResource()}.</li>
   *   <li>Then return {@code (unbound file resource)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toString()}
   */
  @Test
  public void testToString_givenFileResource_thenReturnUnboundFileResource() {
    // Arrange, Act and Assert
    assertEquals("(unbound file resource)", (new FileResource()).toString());
  }

  /**
   * Test {@link Resource#toString()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} Name is {@code foo}.</li>
   *   <li>Then return {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toString()}
   */
  @Test
  public void testToString_givenResourceNameIsFoo_thenReturnFoo() {
    // Arrange
    Resource resource = new Resource();
    resource.setName("foo");

    // Act and Assert
    assertEquals("foo", resource.toString());
  }

  /**
   * Test {@link Resource#toString()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@code (anonymous)}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toString()}
   */
  @Test
  public void testToString_givenResource_thenReturnAnonymous() {
    // Arrange, Act and Assert
    assertEquals("(anonymous)", (new Resource()).toString());
  }

  /**
   * Test {@link Resource#toString()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()}.</li>
   *   <li>Then return {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toString()}
   */
  @Test
  public void testToString_givenStringResource_thenReturnNull() {
    // Arrange, Act and Assert
    assertEquals("null", (new StringResource()).toString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   *   <li>Then return {@code Resource "(anonymous)"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_givenJavaLangObject_thenReturnResourceAnonymous() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.ComponentHelper", typeClass);

    Resource resource = new Resource();
    resource.setProject(project);

    // Act and Assert
    assertEquals("Resource \"(anonymous)\"", resource.toLongString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource()} Name is {@code Name}.</li>
   *   <li>Then return {@code PropertyResource "null"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_givenPropertyResourceNameIsName_thenReturnPropertyResourceNull() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource();
    propertyResource.setName("Name");

    // Act and Assert
    assertEquals("PropertyResource \"null\"", propertyResource.toLongString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Given {@link PropertyResource#PropertyResource(Project, String)} with p is {@link Project} (default constructor) and n is {@code foo} Name is {@code Name}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_givenPropertyResourceWithPIsProjectAndNIsFooNameIsName() {
    // Arrange
    PropertyResource propertyResource = new PropertyResource(new Project(), "foo");
    propertyResource.setName("Name");

    // Act and Assert
    assertEquals("PropertyResource \"null\"", propertyResource.toLongString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} Name is {@code foo}.</li>
   *   <li>Then return {@code Resource "foo"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_givenResourceNameIsFoo_thenReturnResourceFoo() {
    // Arrange
    Resource resource = new Resource();
    resource.setName("foo");

    // Act and Assert
    assertEquals("Resource \"foo\"", resource.toLongString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()} Project is {@link Project} (default constructor).</li>
   *   <li>Then return {@code Resource "(anonymous)"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_givenResourceProjectIsProject_thenReturnResourceAnonymous() {
    // Arrange
    Resource resource = new Resource();
    resource.setProject(new Project());

    // Act and Assert
    assertEquals("Resource \"(anonymous)\"", resource.toLongString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then return {@code Resource "(anonymous)"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_givenResource_thenReturnResourceAnonymous() {
    // Arrange, Act and Assert
    assertEquals("Resource \"(anonymous)\"", (new Resource()).toLongString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Given {@link StringResource#StringResource()} Name is {@code Name}.</li>
   *   <li>Then return {@code StringResource "Name"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_givenStringResourceNameIsName_thenReturnStringResourceName() {
    // Arrange
    StringResource stringResource = new StringResource();
    stringResource.setName("Name");

    // Act and Assert
    assertEquals("StringResource \"Name\"", stringResource.toLongString());
  }

  /**
   * Test {@link Resource#toLongString()}.
   * <ul>
   *   <li>Then return {@code FileResource "(unbound file resource)"}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#toLongString()}
   */
  @Test
  public void testToLongString_thenReturnFileResourceUnboundFileResource() {
    // Arrange
    FileResource fileResource = new FileResource();
    fileResource.setName("Name");

    // Act and Assert
    assertEquals("FileResource \"(unbound file resource)\"", fileResource.toLongString());
  }

  /**
   * Test {@link Resource#setRefid(Reference)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>Then not {@link Resource#Resource()} Checked.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#setRefid(Reference)}
   */
  @Test
  public void testSetRefid_givenResource_thenNotResourceChecked() {
    // Arrange
    Resource resource = new Resource();
    Reference r = new Reference("42");

    // Act
    resource.setRefid(r);

    // Assert
    assertFalse(resource.isChecked());
    assertTrue(resource.isReference());
    assertSame(r, resource.getRefid());
  }

  /**
   * Test {@link Resource#as(Class)}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor) Checked is {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#as(Class)}
   */
  @Test
  public void testAs_givenMappedResourceWithRIsResourceAndMIsCutDirsMapperCheckedIsTrue() {
    // Arrange
    Resource r = new Resource();

    MappedResource mappedResource = new MappedResource(r, new CutDirsMapper());
    mappedResource.setChecked(true);
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(r, mappedResource.as(clazz));
  }

  /**
   * Test {@link Resource#as(Class)}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   *   <li>Then return {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#as(Class)}
   */
  @Test
  public void testAs_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper_thenReturnResource() {
    // Arrange
    Resource r = new Resource();
    MappedResource mappedResource = new MappedResource(r, new CutDirsMapper());
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(r, mappedResource.as(clazz));
  }

  /**
   * Test {@link Resource#as(Class)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@code Object}.</li>
   *   <li>Then return {@link Resource#Resource()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#as(Class)}
   */
  @Test
  public void testAs_givenResource_whenJavaLangObject_thenReturnResource() {
    // Arrange
    Resource resource = new Resource();
    Class<Object> clazz = Object.class;

    // Act and Assert
    assertSame(resource, resource.as(clazz));
  }

  /**
   * Test {@link Resource#asOptional(Class)}.
   * <p>
   * Method under test: {@link Resource#asOptional(Class)}
   */
  @Test
  public void testAsOptional() {
    // Arrange
    Resource r = new Resource();

    MappedResource mappedResource = new MappedResource(r, new CutDirsMapper());
    mappedResource.setChecked(true);
    Class<Object> clazz = Object.class;

    // Act
    Optional<Object> actualAsOptionalResult = mappedResource.asOptional(clazz);

    // Assert
    assertTrue(actualAsOptionalResult.isPresent());
    assertSame(r, actualAsOptionalResult.get());
  }

  /**
   * Test {@link Resource#asOptional(Class)}.
   * <ul>
   *   <li>Given {@link MappedResource#MappedResource(Resource, FileNameMapper)} with r is {@link Resource#Resource()} and m is {@link CutDirsMapper} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#asOptional(Class)}
   */
  @Test
  public void testAsOptional_givenMappedResourceWithRIsResourceAndMIsCutDirsMapper() {
    // Arrange
    Resource r = new Resource();
    MappedResource mappedResource = new MappedResource(r, new CutDirsMapper());
    Class<Object> clazz = Object.class;

    // Act
    Optional<Object> actualAsOptionalResult = mappedResource.asOptional(clazz);

    // Assert
    assertTrue(actualAsOptionalResult.isPresent());
    assertSame(r, actualAsOptionalResult.get());
  }

  /**
   * Test {@link Resource#asOptional(Class)}.
   * <ul>
   *   <li>Given {@link Resource#Resource()}.</li>
   *   <li>When {@code Object}.</li>
   *   <li>Then return Present.</li>
   * </ul>
   * <p>
   * Method under test: {@link Resource#asOptional(Class)}
   */
  @Test
  public void testAsOptional_givenResource_whenJavaLangObject_thenReturnPresent() {
    // Arrange
    Resource resource = new Resource();
    Class<Object> clazz = Object.class;

    // Act
    Optional<Object> actualAsOptionalResult = resource.asOptional(clazz);

    // Assert
    assertTrue(actualAsOptionalResult.isPresent());
    assertSame(resource, actualAsOptionalResult.get());
  }
}
