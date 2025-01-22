package org.apache.tools.ant.types.resources.comparators;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotEquals;
import java.nio.file.Paths;
import org.apache.tools.ant.types.Resource;
import org.apache.tools.ant.types.resources.FileResource;
import org.junit.Test;

public class ResourceComparatorDiffblueTest {
  /**
   * Test {@link ResourceComparator#compare(Resource, Resource)} with {@code Resource}, {@code Resource}.
   * <p>
   * Method under test: {@link ResourceComparator#compare(Resource, Resource)}
   */
  @Test
  public void testCompareWithResourceResource() {
    // Arrange
    Content content = new Content();
    FileResource foo = new FileResource(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertEquals(-1, content.compare(foo, new Resource()));
  }

  /**
   * Test {@link ResourceComparator#compare(Resource, Resource)} with {@code Resource}, {@code Resource}.
   * <ul>
   *   <li>Given {@code file attribute is null!}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceComparator#compare(Resource, Resource)}
   */
  @Test
  public void testCompareWithResourceResource_givenFileAttributeIsNull() {
    // Arrange
    Content content = new Content();

    FileResource foo = new FileResource();
    foo.setName("file attribute is null!");

    // Act and Assert
    assertEquals(-1, content.compare(foo, new Resource()));
  }

  /**
   * Test {@link ResourceComparator#equals(Object)}, and {@link ResourceComparator#hashCode()}.
   * <ul>
   *   <li>When other is equal.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ResourceComparator#equals(Object)}
   *   <li>{@link ResourceComparator#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsEqual_thenReturnEqual() {
    // Arrange
    Content content = new Content();
    Content content2 = new Content();

    // Act and Assert
    assertEquals(content, content2);
    int expectedHashCodeResult = content.hashCode();
    assertEquals(expectedHashCodeResult, content2.hashCode());
  }

  /**
   * Test {@link ResourceComparator#equals(Object)}, and {@link ResourceComparator#hashCode()}.
   * <ul>
   *   <li>When other is same.</li>
   *   <li>Then return equal.</li>
   * </ul>
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link ResourceComparator#equals(Object)}
   *   <li>{@link ResourceComparator#hashCode()}
   * </ul>
   */
  @Test
  public void testEqualsAndHashCode_whenOtherIsSame_thenReturnEqual() {
    // Arrange
    Content content = new Content();

    // Act and Assert
    assertEquals(content, content);
    int expectedHashCodeResult = content.hashCode();
    assertEquals(expectedHashCodeResult, content.hashCode());
  }

  /**
   * Test {@link ResourceComparator#equals(Object)}.
   * <ul>
   *   <li>When other is different.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceComparator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsDifferent_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Content(), 1);
  }

  /**
   * Test {@link ResourceComparator#equals(Object)}.
   * <ul>
   *   <li>When other is {@code null}.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceComparator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsNull_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Content(), null);
  }

  /**
   * Test {@link ResourceComparator#equals(Object)}.
   * <ul>
   *   <li>When other is wrong type.</li>
   *   <li>Then return not equal.</li>
   * </ul>
   * <p>
   * Method under test: {@link ResourceComparator#equals(Object)}
   */
  @Test
  public void testEquals_whenOtherIsWrongType_thenReturnNotEqual() {
    // Arrange, Act and Assert
    assertNotEquals(new Content(), "Different type to ResourceComparator");
  }
}
