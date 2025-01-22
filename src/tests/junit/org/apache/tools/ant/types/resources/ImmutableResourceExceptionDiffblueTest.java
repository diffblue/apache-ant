package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class ImmutableResourceExceptionDiffblueTest {
  /**
   * Test {@link ImmutableResourceException#ImmutableResourceException()}.
   * <ul>
   *   <li>Then return Message is {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImmutableResourceException#ImmutableResourceException()}
   */
  @Test
  public void testNewImmutableResourceException_thenReturnMessageIsNull() {
    // Arrange and Act
    ImmutableResourceException actualImmutableResourceException = new ImmutableResourceException();

    // Assert
    assertNull(actualImmutableResourceException.getMessage());
    assertNull(actualImmutableResourceException.getCause());
    assertEquals(0, actualImmutableResourceException.getSuppressed().length);
  }

  /**
   * Test {@link ImmutableResourceException#ImmutableResourceException(String)}.
   * <ul>
   *   <li>When {@code foo}.</li>
   *   <li>Then return Message is {@code foo}.</li>
   * </ul>
   * <p>
   * Method under test: {@link ImmutableResourceException#ImmutableResourceException(String)}
   */
  @Test
  public void testNewImmutableResourceException_whenFoo_thenReturnMessageIsFoo() {
    // Arrange and Act
    ImmutableResourceException actualImmutableResourceException = new ImmutableResourceException("foo");

    // Assert
    assertEquals("foo", actualImmutableResourceException.getMessage());
    assertNull(actualImmutableResourceException.getCause());
    assertEquals(0, actualImmutableResourceException.getSuppressed().length);
  }
}
