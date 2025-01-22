package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import java.nio.file.Paths;
import java.util.NoSuchElementException;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class FileResourceIteratorDiffblueTest {
  /**
   * Test {@link FileResourceIterator#addFiles(String[])}.
   * <ul>
   *   <li>Then {@link FileResourceIterator#FileResourceIterator()} next.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#addFiles(String[])}
   */
  @Test
  public void testAddFiles_thenFileResourceIteratorNext() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{"foo"});

    // Act
    fileResourceIterator.addFiles(new String[]{"foo"});

    // Assert
    Resource nextResult = fileResourceIterator.next();
    assertTrue(nextResult instanceof FileResource);
    Resource nextResult2 = fileResourceIterator.next();
    assertTrue(nextResult2 instanceof FileResource);
    assertFalse(fileResourceIterator.hasNext());
    assertEquals(nextResult, nextResult2);
  }

  /**
   * Test {@link FileResourceIterator#hasNext()}.
   * <ul>
   *   <li>Given {@link FileResourceIterator#FileResourceIterator()} addFiles array of {@link String} with {@code foo}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#hasNext()}
   */
  @Test
  public void testHasNext_givenFileResourceIteratorAddFilesArrayOfStringWithFoo_thenReturnTrue() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{"foo"});

    // Act and Assert
    assertTrue(fileResourceIterator.hasNext());
  }

  /**
   * Test {@link FileResourceIterator#hasNext()}.
   * <ul>
   *   <li>Given {@link FileResourceIterator#FileResourceIterator()} addFiles empty array of {@link String}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#hasNext()}
   */
  @Test
  public void testHasNext_givenFileResourceIteratorAddFilesEmptyArrayOfString_thenReturnFalse() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{});

    // Act and Assert
    assertFalse(fileResourceIterator.hasNext());
  }

  /**
   * Test {@link FileResourceIterator#next()}.
   * <ul>
   *   <li>Then {@link FileResourceIterator#FileResourceIterator()} next {@link FileResource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#next()}
   */
  @Test
  public void testNext_thenFileResourceIteratorNextFileResource() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{"."});
    fileResourceIterator.addFiles(new String[]{"foo"});

    // Act
    fileResourceIterator.next();

    // Assert
    Resource nextResult = fileResourceIterator.next();
    assertTrue(nextResult instanceof FileResource);
    assertEquals("foo", ((FileResource) nextResult).getFile().getName());
    assertEquals("foo", nextResult.getName());
    assertEquals(0L, nextResult.getLastModified());
    assertEquals(0L, nextResult.getSize());
    assertFalse(nextResult.isDirectory());
    assertFalse(fileResourceIterator.hasNext());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "foo").toString(), "\"");
    assertEquals(expectedToLongStringResult, nextResult.toLongString());
  }

  /**
   * Test {@link FileResourceIterator#next()}.
   * <ul>
   *   <li>Then return File Name is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#next()}
   */
  @Test
  public void testNext_thenReturnFileNameIsFoo() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{"foo"});

    // Act
    Resource actualNextResult = fileResourceIterator.next();

    // Assert
    assertTrue(actualNextResult instanceof FileResource);
    assertEquals("foo", ((FileResource) actualNextResult).getFile().getName());
    assertEquals("foo", actualNextResult.getName());
    assertEquals(0L, actualNextResult.getLastModified());
    assertEquals(0L, actualNextResult.getSize());
    assertFalse(actualNextResult.isDirectory());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "foo").toString(), "\"");
    assertEquals(expectedToLongStringResult, actualNextResult.toLongString());
  }

  /**
   * Test {@link FileResourceIterator#next()}.
   * <ul>
   *   <li>Then throw {@link NoSuchElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#next()}
   */
  @Test
  public void testNext_thenThrowNoSuchElementException() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{});

    // Act and Assert
    assertThrows(NoSuchElementException.class, () -> fileResourceIterator.next());
  }

  /**
   * Test {@link FileResourceIterator#remove()}.
   * <p>
   * Method under test: {@link FileResourceIterator#remove()}
   */
  @Test
  public void testRemove() {
    // Arrange, Act and Assert
    assertThrows(UnsupportedOperationException.class, () -> (new FileResourceIterator()).remove());
  }

  /**
   * Test {@link FileResourceIterator#nextResource()}.
   * <ul>
   *   <li>Then {@link FileResourceIterator#FileResourceIterator()} next {@link FileResource}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#nextResource()}
   */
  @Test
  public void testNextResource_thenFileResourceIteratorNextFileResource() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{"."});
    fileResourceIterator.addFiles(new String[]{"foo"});

    // Act
    fileResourceIterator.nextResource();

    // Assert
    Resource nextResult = fileResourceIterator.next();
    assertTrue(nextResult instanceof FileResource);
    assertEquals("foo", ((FileResource) nextResult).getFile().getName());
    assertEquals("foo", nextResult.getName());
    assertEquals(0L, nextResult.getLastModified());
    assertEquals(0L, nextResult.getSize());
    assertFalse(nextResult.isDirectory());
    assertFalse(fileResourceIterator.hasNext());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "foo").toString(), "\"");
    assertEquals(expectedToLongStringResult, nextResult.toLongString());
  }

  /**
   * Test {@link FileResourceIterator#nextResource()}.
   * <ul>
   *   <li>Then return File Name is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#nextResource()}
   */
  @Test
  public void testNextResource_thenReturnFileNameIsFoo() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{"foo"});

    // Act
    FileResource actualNextResourceResult = fileResourceIterator.nextResource();

    // Assert
    assertEquals("foo", actualNextResourceResult.getFile().getName());
    assertEquals("foo", actualNextResourceResult.getName());
    assertEquals(0L, actualNextResourceResult.getLastModified());
    assertEquals(0L, actualNextResourceResult.getSize());
    assertFalse(actualNextResourceResult.isDirectory());
    String expectedToLongStringResult = String.join("", "FileResource \"",
        Paths.get(System.getProperty("user.dir"), "foo").toString(), "\"");
    assertEquals(expectedToLongStringResult, actualNextResourceResult.toLongString());
  }

  /**
   * Test {@link FileResourceIterator#nextResource()}.
   * <ul>
   *   <li>Then throw {@link NoSuchElementException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link FileResourceIterator#nextResource()}
   */
  @Test
  public void testNextResource_thenThrowNoSuchElementException() {
    // Arrange
    FileResourceIterator fileResourceIterator = new FileResourceIterator();
    fileResourceIterator.addFiles(new String[]{});

    // Act and Assert
    assertThrows(NoSuchElementException.class, () -> fileResourceIterator.nextResource());
  }
}
