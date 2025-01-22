package org.apache.tools.ant.types.resources.selectors;

import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.types.Comparison;
import org.apache.tools.ant.types.Resource;
import org.junit.Test;

public class SizeDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link Size#setSize(long)}
   *   <li>{@link Size#setWhen(Comparison)}
   *   <li>{@link Size#getSize()}
   *   <li>{@link Size#getWhen()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange
    Size size = new Size();

    // Act
    size.setSize(1L);
    size.setWhen(Comparison.EQUAL);
    long actualSize = size.getSize();
    Comparison actualWhen = size.getWhen();

    // Assert
    assertEquals(1L, actualSize);
    assertSame(actualWhen.EQUAL, actualWhen);
  }

  /**
   * Test {@link Size#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Size} (default constructor) Size is one.</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Size#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenSizeSizeIsOne_whenResource_thenReturnFalse() {
    // Arrange
    Size size = new Size();
    size.setSize(1L);

    // Act and Assert
    assertFalse(size.isSelected(new Resource()));
  }

  /**
   * Test {@link Size#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Size} (default constructor).</li>
   *   <li>When {@link Resource#Resource(String)} with {@code Name}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Size#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenSize_whenResourceWithName_thenReturnFalse() {
    // Arrange
    Size size = new Size();

    // Act and Assert
    assertFalse(size.isSelected(new Resource("Name")));
  }

  /**
   * Test {@link Size#isSelected(Resource)}.
   * <ul>
   *   <li>Given {@link Size} (default constructor).</li>
   *   <li>When {@link Resource#Resource()}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Size#isSelected(Resource)}
   */
  @Test
  public void testIsSelected_givenSize_whenResource_thenReturnTrue() {
    // Arrange
    Size size = new Size();

    // Act and Assert
    assertTrue(size.isSelected(new Resource()));
  }

  /**
   * Test new {@link Size} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Size}
   */
  @Test
  public void testNewSize() {
    // Arrange and Act
    Size actualSize = new Size();

    // Assert
    Comparison resultWhen = actualSize.getWhen();
    assertEquals("equal", resultWhen.getValue());
    assertEquals(-1L, actualSize.getSize());
    assertEquals(0, resultWhen.getIndex());
    assertArrayEquals(new String[]{"equal", "greater", "less", "ne", "ge", "le", "eq", "gt", "lt", "more"},
        resultWhen.getValues());
  }
}
